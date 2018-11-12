module Tree exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , openLineage
    , update
    , view
    , viewBreadcrumbs
    )

import Api exposing (ApiError)
import Dict exposing (Dict)
import Dict.Extra
import Folder exposing (Folder, FolderCounts, FolderId)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Route
import Utils


type Return
    = NoReturn
    | GotRootFolders (List Folder)
    | UserSelection Folder


type alias Model =
    { folderCache : Dict FolderId FolderInTree
    , rootIds : List FolderId
    , loading : Int
    , error : Maybe ApiError
    , selection : List FolderId
    , showSubselection : Bool
    }


type alias FolderInTree =
    { folder : Folder
    , subLinks : Maybe (List FolderId)
    }


type Msg
    = ApiResponseSubfolder (Api.Response (List Folder))
    | ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))
    | Select FolderId


init : ( Model, Cmd Msg )
init =
    ( { folderCache = Dict.empty
      , rootIds = []
      , loading = 1
      , error = Nothing
      , selection = []
      , showSubselection = True
      }
    , Api.makeQueryRequest
        ApiResponseToplevelFolder
        Api.queryToplevelFolder
    )


selectedFolder : Model -> Maybe Folder
selectedFolder model =
    List.head model.selection
        |> Maybe.andThen (\a -> Dict.get a model.folderCache)
        |> Maybe.map .folder


openLineage : Nonempty Folder -> Model -> ( Model, Cmd Msg )
openLineage lineage model =
    let
        offspring =
            List.Nonempty.head lineage

        lineageAsList =
            List.Nonempty.toList lineage
    in
    model
        |> addFolders lineageAsList
        |> selectFolder offspring.id
        |> loadSubfolders (List.map .id lineageAsList)


update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    case msg of
        ApiResponseToplevelFolder (Err err) ->
            ( { model
                | loading = model.loading - 1
                , error = Just err
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseSubfolder (Err err) ->
            ( { model
                | loading = model.loading - 1
                , error = Just err
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseToplevelFolder (Ok listOfRootFoldersWithSubfolders) ->
            ( { model
                | loading = model.loading - 1
                , error = Nothing
              }
                |> addRootFolders listOfRootFoldersWithSubfolders
            , Cmd.none
            , GotRootFolders
                (List.map Tuple.first listOfRootFoldersWithSubfolders)
            )

        ApiResponseSubfolder (Ok folderList) ->
            ( { model
                | loading = model.loading - 1
                , error = Nothing
              }
                |> addFolders folderList
                |> linkAsSubfolders folderList
            , Cmd.none
            , NoReturn
            )

        Select id ->
            model
                |> selectFolder id
                |> loadSubfolders [ id ]
                |> (\( model1, cmd1 ) ->
                        ( model1
                        , cmd1
                        , if List.head model.selection /= Just id then
                            Maybe.Extra.unwrap
                                NoReturn
                                UserSelection
                                (selectedFolder model1)

                          else
                            NoReturn
                        )
                   )


addRootFolders : List ( Folder, List Folder ) -> Model -> Model
addRootFolders rootFoldersWithSubfolders model =
    let
        modelWithFoldersAdded : Model
        modelWithFoldersAdded =
            List.foldl
                addRootFolder
                model
                rootFoldersWithSubfolders

        addRootFolder : ( Folder, List Folder ) -> Model -> Model
        addRootFolder ( rootFolder, subFolders ) model1 =
            model1
                |> addFolders (rootFolder :: subFolders)
                |> linkAsSubfolders subFolders

        rootIds : List FolderId
        rootIds =
            List.map (Tuple.first >> .id) rootFoldersWithSubfolders
    in
    { modelWithFoldersAdded
        | rootIds = rootIds
        , selection =
            if List.isEmpty model.selection then
                List.take 1 rootIds

            else
                model.selection
    }


addFolders : List Folder -> Model -> Model
addFolders folderList model =
    let
        newFolderCache =
            List.foldl
                (\folder dict ->
                    Dict.Extra.insertDedupe
                        (\org new ->
                            { folder = new.folder
                            , subLinks = Maybe.Extra.or new.subLinks org.subLinks
                            }
                        )
                        folder.id
                        { folder = folder
                        , subLinks =
                            if Folder.hasSubfolder folder then
                                Nothing

                            else
                                Just []
                        }
                        dict
                )
                model.folderCache
                folderList
    in
    { model | folderCache = newFolderCache }


linkAsSubfolders : List Folder -> Model -> Model
linkAsSubfolders allSubfoldersOfSomeParents model =
    { model
        | folderCache =
            Dict.Extra.filterGroupBy
                .parent
                allSubfoldersOfSomeParents
                |> Dict.toList
                |> List.foldl
                    (\( parentId, subfolders ) ->
                        Dict.update
                            parentId
                            (Maybe.map
                                (\folderInTree ->
                                    { folderInTree
                                        | subLinks = Just (List.map .id subfolders)
                                    }
                                )
                            )
                    )
                    model.folderCache
    }


getSubLinks : FolderId -> Model -> Maybe (List FolderId)
getSubLinks id model =
    Dict.get id model.folderCache
        |> Maybe.andThen .subLinks


getParentId : FolderId -> Model -> Maybe FolderId
getParentId id model =
    Dict.get id model.folderCache
        |> Maybe.andThen (.folder >> .parent)


getPath : FolderId -> Model -> List FolderId
getPath id model =
    id
        :: (case getParentId id model of
                Nothing ->
                    []

                Just parentId ->
                    getPath parentId model
           )


selectFolder : FolderId -> Model -> Model
selectFolder id model =
    let
        alreadySelected =
            List.head model.selection == Just id
    in
    { model
        | selection = getPath id model
        , showSubselection = not alreadySelected || not model.showSubselection
    }


loadSubfolders : List FolderId -> Model -> ( Model, Cmd Msg )
loadSubfolders parentIds model =
    let
        parentIdsWithUnknownChildren =
            List.filter
                (\parentId -> getSubLinks parentId model == Nothing)
                parentIds
    in
    if List.isEmpty parentIdsWithUnknownChildren then
        ( model, Cmd.none )

    else
        ( { model | loading = model.loading + 1 }
        , Api.makeQueryRequest
            ApiResponseSubfolder
            (Api.querySubfolder parentIdsWithUnknownChildren)
        )


view : Model -> FolderCounts -> Html Msg
view model folderCounts =
    Html.div []
        [ viewListOfFolders model folderCounts model.rootIds ]


viewListOfFolders : Model -> FolderCounts -> List FolderId -> Html Msg
viewListOfFolders model folderCounts folderIds =
    Html.ul [ Html.Attributes.class "folder-list" ] <|
        List.map
            (\id ->
                Html.li []
                    [ viewFolder model folderCounts id ]
            )
            folderIds


viewListOfFoldersLoading : Html Msg
viewListOfFoldersLoading =
    Html.ul [ Html.Attributes.class "folder-list" ]
        [ Html.li [] [ Html.text "..." ]
        ]


viewFolder : Model -> FolderCounts -> FolderId -> Html Msg
viewFolder model folderCounts id =
    let
        isSelectedFolder =
            List.head model.selection == Just id

        expanded =
            List.member id model.selection
                && (not isSelectedFolder || model.showSubselection)
    in
    case Dict.get id model.folderCache of
        Nothing ->
            -- Cache miss. Should never happen,
            -- because only cached folders are getting linked.
            Html.div [] [ Html.text "..." ]

        Just { folder } ->
            Html.div []
                [ Html.div
                    [ Html.Events.onClick (Select id) ]
                    [ Folder.view
                        folder
                        (Dict.get folder.id folderCounts)
                        isSelectedFolder
                        expanded
                    ]
                , if expanded then
                    case getSubLinks id model of
                        Nothing ->
                            viewListOfFoldersLoading

                        Just subLinks ->
                            viewListOfFolders model folderCounts subLinks

                  else
                    Html.text ""
                ]


viewBreadcrumbs : Model -> FolderId -> Html msg
viewBreadcrumbs model id =
    Html.span []
        (getPath id model
            |> List.reverse
            |> List.map
                (\id1 ->
                    Html.span []
                        [ case Dict.get id1 model.folderCache of
                            Just { folder } ->
                                Html.a
                                    [ folder.id
                                        |> Folder.idToInt
                                        |> Route.NodeId
                                        |> Route.toString
                                        |> Html.Attributes.href
                                    ]
                                    [ Html.text folder.name ]

                            Nothing ->
                                Html.text "..."
                        ]
                )
            |> List.intersperse
                (Html.span [] [ Html.text " > " ])
        )
