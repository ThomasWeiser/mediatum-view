module Tree
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        , viewBreadcrumbs
        , selectedFolder
        )

import Dict exposing (Dict)
import Maybe.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Folder exposing (FolderId, Folder, FolderCounts)
import Api exposing (ApiError)


type alias Model =
    { folderCache : Dict FolderId FolderInTree
    , rootIds : List FolderId
    , loading : Bool
    , error : Maybe ApiError
    , selection : List FolderId
    , showSubselection : Bool
    }


type alias FolderInTree =
    { folder : Folder
    , subLinks : Maybe (List FolderId)
    }


type Msg
    = ApiResponseSubfolder FolderId (Api.Response (List Folder))
    | ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))
    | Select FolderId


init : ( Model, Cmd Msg )
init =
    ( { folderCache = Dict.empty
      , rootIds = []
      , loading = True
      , error = Nothing
      , selection = []
      , showSubselection = True
      }
    , Api.makeRequest
        ApiResponseToplevelFolder
        Api.queryToplevelFolder
    )


selectedFolder : Model -> Maybe Folder
selectedFolder model =
    List.head model.selection
        |> Maybe.andThen (flip Dict.get model.folderCache)
        |> Maybe.map .folder


update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg model =
    (case msg of
        ApiResponseToplevelFolder (Err err) ->
            ( { model
                | loading = False
                , error = Just err
              }
            , Cmd.none
            )

        ApiResponseSubfolder _ (Err err) ->
            ( { model
                | loading = False
                , error = Just err
              }
            , Cmd.none
            )

        ApiResponseToplevelFolder (Ok listOfRootFoldersWithSubfolders) ->
            ( { model
                | loading = False
                , error = Nothing
              }
                |> addRootFolders listOfRootFoldersWithSubfolders
            , Cmd.none
            )

        ApiResponseSubfolder superfolderId (Ok folderList) ->
            ( { model
                | loading = False
                , error = Nothing
              }
                |> addFolders folderList
                |> setSubfolders superfolderId folderList
            , Cmd.none
            )

        Select id ->
            model
                |> selectFolder id
                |> loadSubfolder id
    )
        |> (\( newModel, cmd ) ->
                ( newModel
                , cmd
                , newModel.selection /= model.selection
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
                |> setSubfolders rootFolder.id subFolders

        rootIds : List FolderId
        rootIds =
            List.map (Tuple.first >> .id) rootFoldersWithSubfolders
    in
        { modelWithFoldersAdded
            | rootIds = rootIds
            , selection = List.take 1 rootIds
        }


addFolders : List Folder -> Model -> Model
addFolders folderList model =
    let
        newFolderCache =
            List.foldl
                (\folder dict ->
                    Dict.insert
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


setSubfolders : FolderId -> List Folder -> Model -> Model
setSubfolders id folderList model =
    { model
        | folderCache =
            Dict.update
                id
                (Maybe.map
                    (\folderInTree ->
                        { folderInTree
                            | subLinks = Just (List.map .id folderList)
                        }
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


loadSubfolder : FolderId -> Model -> ( Model, Cmd Msg )
loadSubfolder superfolderId model =
    case getSubLinks superfolderId model of
        Nothing ->
            ( { model | loading = True }
            , Api.makeRequest
                (ApiResponseSubfolder superfolderId)
                (Api.querySubfolder superfolderId)
            )

        Just _ ->
            ( model, Cmd.none )


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
                        [ Dict.get id1 model.folderCache
                            |> Maybe.Extra.unwrap "..." (.folder >> .name)
                            |> Html.text
                        ]
                )
            |> List.intersperse
                (Html.span [] [ Html.text " > " ])
        )
