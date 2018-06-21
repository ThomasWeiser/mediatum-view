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
            loadSubfolder id (toggleFolder id model)


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
        { model | foldersById = newFoldersById } |> Debug.log "foldersById"


setToplevelIds : List Folder -> Model -> Model
setToplevelIds folderList model =
    { model
        | toplevelIds =
            List.map .id folderList
                |> Debug.log "toplevelIds"
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


toggleFolder : FolderId -> Model -> Model
toggleFolder id model =
    { model
        | foldersById =
            Dict.update
                id
                (Maybe.map Folder.toggle)
                model.foldersById
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
    case Dict.get id model.foldersById of
        Nothing ->
            Html.div [] [ Html.text "..." ]

        Just folder ->
            Html.div []
                [ Html.div
                    [ Html.Events.onClick (Select id) ]
                    [ Folder.view folder ]
                , if folder.isExpanded then
                    case getSubfolderIds id model of
                        Nothing ->
                            Html.text ""

                        Just subfolderIds ->
                            viewListOfFolders model subfolderIds
                  else
                    Html.text ""
                ]
