module Tree
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        , selectedFolderId
        )

import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Graphqelm.Http
import Folder exposing (FolderId, Folder)
import Api


type alias Model =
    { foldersById : Dict FolderId Folder
    , rootIds : List FolderId
    , loading : Bool
    , error : Maybe (Graphqelm.Http.Error (List Folder))
    , selection : List FolderId
    , showSubselection : Bool
    }


type Msg
    = ApiResponse Bool (Api.Response (List Folder))
    | Select FolderId


init : ( Model, Cmd Msg )
init =
    ( { foldersById = Dict.empty
      , rootIds = []
      , loading = True
      , error = Nothing
      , selection = []
      , showSubselection = True
      }
    , Api.makeRequest
        (ApiResponse True)
        Api.queryToplevelFolder
    )


selectedFolderId : Model -> Maybe FolderId
selectedFolderId model=
    List.head model.selection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponse _ (Err err) ->
            ( { model
                | loading = False
                , error = Just err
              }
            , Cmd.none
            )

        ApiResponse isRootQuery (Ok folderList) ->
            ( { model
                | loading = False
                , error = Nothing
              }
                |> addFolders folderList
                |> (if isRootQuery then setRootIds folderList else identity)
                |> setSubfolders folderList
            , Cmd.none
            )

        Select id ->
            model
                |> selectFolder id
                |> loadSubfolder id


addFolders : List Folder -> Model -> Model
addFolders folderList model =
    let
        newFoldersById =
            List.foldl
                (\folder dict ->
                    Dict.insert
                        folder.id
                        folder
                        dict
                )
                model.foldersById
                folderList
    in
        { model | foldersById = newFoldersById }


setRootIds : List Folder -> Model -> Model
setRootIds rootFolderList model =
    { model
        | rootIds =
            List.filterMap
                (\folder ->
                    if Folder.isRoot folder then
                        Just folder.id
                    else
                        Nothing
                )
                rootFolderList
        , selection =
             List.take 1 rootFolderList |> List.map .id
    }


setSubfolders : List Folder -> Model -> Model
setSubfolders folderList model =
    let
        groupedFolderList : Dict FolderId (List Folder)
        groupedFolderList = Dict.Extra.filterGroupBy
                .parent
                folderList

        newFoldersById =
            Dict.foldl
                (\parentId subfolders dict ->
                    Dict.update
                        parentId
                        (Maybe.map
                            (\parentFolder ->
                                { parentFolder
                                | subfolderIds = Just (List.map .id subfolders)
                                }
                            )
                        )
                        dict
                )
                model.foldersById
                groupedFolderList

    in
        { model | foldersById = newFoldersById }


getSubfolders : FolderId -> Model -> Maybe (List Folder)
getSubfolders id model =
    case getSubfolderIds id model of
        Just subfolderIdList ->
            Just <|
                List.filterMap
                    (flip Dict.get model.foldersById)
                    subfolderIdList

        Nothing ->
            Nothing


getSubfolderIds : FolderId -> Model -> Maybe (List FolderId)
getSubfolderIds id model =
    case Dict.get id model.foldersById of
        Just superFolder ->
            superFolder.subfolderIds

        Nothing ->
            Nothing


getParentId : FolderId -> Model -> Maybe FolderId
getParentId id model =
    Dict.get id model.foldersById
        |> Maybe.andThen .parent


selectFolder : FolderId -> Model -> Model
selectFolder id model =
    let
        selectionPath : FolderId -> List FolderId
        selectionPath id =
            id
                :: (case getParentId id model of
                        Nothing ->
                            []

                        Just parentId ->
                            selectionPath parentId
                   )

        alreadySelected =
            List.head model.selection == Just id
    in
        { model
            | selection = selectionPath id
            , showSubselection = not alreadySelected || not model.showSubselection
        }


loadSubfolder : FolderId -> Model -> ( Model, Cmd Msg )
loadSubfolder id model =
    case getSubfolderIds id model of
        Nothing ->
            ( { model | loading = True }
            , Api.makeRequest
                (ApiResponse False)
                (Api.querySubfolder id)
            )

        Just _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewListOfFolders model model.rootIds ]


viewListOfFolders : Model -> List FolderId -> Html Msg
viewListOfFolders model folderIds =
    Html.ul [ Html.Attributes.class "folder-list" ] <|
        List.map
            (\id ->
                Html.li []
                    [ viewFolder model id ]
            )
            folderIds


viewFolder : Model -> FolderId -> Html Msg
viewFolder model id =
    let
        isSelectedFolder =
            List.head model.selection == Just id

        expanded =
            List.member id model.selection
                && (not isSelectedFolder || model.showSubselection)
    in
        case Dict.get id model.foldersById of
            Nothing ->
                Html.div [] [ Html.text "..." ]

            Just folder ->
                Html.div []
                    [ Html.div
                        [ Html.Events.onClick (Select id) ]
                        [ Folder.view folder isSelectedFolder expanded ]
                    , if expanded then
                        case getSubfolderIds id model of
                            Nothing ->
                                Html.text ""

                            Just subfolderIds ->
                                viewListOfFolders model subfolderIds
                      else
                        Html.text ""
                    ]
