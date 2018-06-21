module Tree
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Graphqelm.Http
import Folder exposing (FolderId, Folder)
import Api


type alias Model =
    { foldersById : Dict FolderId Folder
    , toplevelIds : List FolderId
    , loading : Bool
    , error : Maybe (Graphqelm.Http.Error (List Folder))
    , selection : List FolderId
    , showSubselection : Bool
    }


type Msg
    = ApiResponse (Maybe FolderId) (Api.Response (List Folder))
    | Select FolderId


init : ( Model, Cmd Msg )
init =
    ( { foldersById = Dict.empty
      , toplevelIds = []
      , loading = True
      , error = Nothing
      , selection = []
      , showSubselection = True
      }
    , Api.makeRequest
        (ApiResponse Nothing)
        Api.queryToplevelFolder
    )


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

        ApiResponse superFolder (Ok folderList) ->
            ( { model
                | loading = False
                , error = Nothing
              }
                |> addFolders folderList
                |> case superFolder of
                    Nothing ->
                        setToplevelIds folderList

                    Just superFolderId ->
                        setSubfolders superFolderId folderList
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


setToplevelIds : List Folder -> Model -> Model
setToplevelIds folderList model =
    { model
        | toplevelIds =
            List.map .id folderList
    }


setSubfolders : FolderId -> List Folder -> Model -> Model
setSubfolders id folderList model =
    { model
        | foldersById =
            Dict.update
                id
                (Maybe.map
                    (\folder ->
                        { folder
                            | subfolderIds =
                                Just
                                    (List.map .id folderList)
                        }
                    )
                )
                model.foldersById
    }


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
                (ApiResponse (Just id))
                (Api.querySubfolder id)
            )

        Just _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewListOfFolders model model.toplevelIds ]


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
