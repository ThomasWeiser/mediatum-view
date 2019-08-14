module Data.Cache exposing
    ( ApiData
    , Model
    , Msg(..)
    , Needs(..)
    , Return(..)
    , dictGetApiData
    , initialModel
    , requestNeeds
    , update
    , updateWithModifiedDocument
    )

import Api
import Api.Queries
import Basics.Extra
import Data.Types exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import GenericNode exposing (GenericNode)
import List.Extra
import List.Nonempty exposing (Nonempty)
import RemoteData exposing (RemoteData(..))


type alias ApiData a =
    RemoteData Api.Error a


type Return
    = NoReturn
    | GotRootFolders


type alias Model =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Dict FolderId (ApiData Folder)
    , subfolderIds : Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Dict Int (ApiData NodeType)
    , folderCounts : Dict Selection (ApiData (Dict FolderId Int))
    , docListPages : Dict Selection (Dict Window (ApiData DocumentsPage))
    , documents : Dict DocumentId (ApiData (Maybe Document))
    }


type Needs
    = NeedNothing
    | NeedListOfNeeds (List Needs)
    | NeedRootFolderIds
    | NeedSubfolders (List FolderId)
    | NeedGenericNode Int
    | NeedDocument DocumentId



-- TODO: | NeedFolderCounts Selection
-- TODO: | NeedDocListPage Selection Window


initialModel : Model
initialModel =
    { rootFolderIds = NotAsked
    , folders = Dict.empty
    , subfolderIds = Dict.empty
    , nodeTypes = Dict.empty
    , folderCounts = Dict.empty
    , docListPages = Dict.empty
    , documents = Dict.empty
    }


dictGetApiData : Dict comparable (ApiData value) -> comparable -> ApiData value
dictGetApiData dict key =
    Dict.get key dict
        |> Maybe.withDefault RemoteData.NotAsked


type Msg
    = ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))
    | ApiResponseSubfolder (List FolderId) (Api.Response (List Folder))
    | ApiResponseGenericNode Int (Api.Response GenericNode)
    | ApiResponseDocument DocumentId (Api.Response (Maybe Document))


requestNeeds : Needs -> Model -> ( Model, Cmd Msg )
requestNeeds needs model =
    case needs of
        NeedNothing ->
            ( model, Cmd.none )

        NeedListOfNeeds listOfNeeds ->
            listOfNeeds
                |> List.Extra.mapAccuml
                    (Basics.Extra.flip requestNeeds)
                    model
                |> Tuple.mapSecond Cmd.batch

        NeedRootFolderIds ->
            case model.rootFolderIds of
                NotAsked ->
                    ( { model
                        | rootFolderIds = Loading
                      }
                    , Api.sendQueryRequest
                        ApiResponseToplevelFolder
                        Api.Queries.toplevelFolder
                    )

                _ ->
                    ( model, Cmd.none )

        NeedSubfolders parentIds ->
            let
                parentIdsWithUnknownChildren =
                    List.filter
                        (\parentId ->
                            (Dict.get parentId model.subfolderIds |> Maybe.withDefault NotAsked)
                                == NotAsked
                        )
                        parentIds
            in
            if List.isEmpty parentIdsWithUnknownChildren then
                ( model, Cmd.none )

            else
                ( { model
                    | subfolderIds =
                        List.foldl
                            (\parentId subfolderIds ->
                                Dict.insert parentId Loading subfolderIds
                            )
                            model.subfolderIds
                            parentIdsWithUnknownChildren
                  }
                , Api.sendQueryRequest
                    (ApiResponseSubfolder parentIds)
                    (Api.Queries.subfolder parentIdsWithUnknownChildren)
                )

        NeedGenericNode nodeNumber ->
            case dictGetApiData model.nodeTypes nodeNumber of
                NotAsked ->
                    ( { model
                        | nodeTypes =
                            Dict.insert nodeNumber Loading model.nodeTypes
                      }
                    , Api.sendQueryRequest
                        (ApiResponseGenericNode nodeNumber)
                        (Api.Queries.genericNode nodeNumber)
                    )

                _ ->
                    ( model, Cmd.none )

        NeedDocument documentId ->
            case dictGetApiData model.documents documentId of
                NotAsked ->
                    ( { model
                        | documents =
                            Dict.insert documentId Loading model.documents
                      }
                    , Api.sendQueryRequest
                        (ApiResponseDocument documentId)
                        (Api.Queries.documentDetails documentId)
                    )

                _ ->
                    ( model, Cmd.none )


updateWithModifiedDocument : Document -> Model -> Model
updateWithModifiedDocument document model =
    { model
        | documents =
            Dict.insert document.id (Success (Just document)) model.documents
    }


update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    case msg of
        ApiResponseToplevelFolder (Ok listOfRootFoldersWithSubfolders) ->
            ( let
                allNewFolders =
                    listOfRootFoldersWithSubfolders
                        |> List.map (Basics.Extra.uncurry (::))
                        |> List.concat
              in
              { model
                | rootFolderIds =
                    Success
                        (List.map (Tuple.first >> .id) listOfRootFoldersWithSubfolders)
              }
                |> insertAsFolders allNewFolders
                |> insertAsSubfolderIds allNewFolders
                |> insertFoldersAsNodeTypes allNewFolders
            , Cmd.none
            , GotRootFolders
            )

        ApiResponseToplevelFolder (Err error) ->
            ( { model
                | rootFolderIds = Failure error
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseSubfolder _ (Ok listOfSubfolders) ->
            ( model
                |> insertAsFolders listOfSubfolders
                |> insertAsSubfolderIds listOfSubfolders
                |> insertFoldersAsNodeTypes listOfSubfolders
            , Cmd.none
            , NoReturn
            )

        ApiResponseSubfolder parentIds (Err error) ->
            ( { model
                | subfolderIds =
                    List.foldl
                        (\parentId subfolderIds ->
                            Dict.insert parentId (Failure error) subfolderIds
                        )
                        model.subfolderIds
                        parentIds
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseGenericNode nodeNumber (Ok genericNode) ->
            let
                model1 =
                    model
                        |> insertNodeType nodeNumber (GenericNode.toNodeType genericNode)
            in
            case genericNode of
                GenericNode.IsFolder lineage ->
                    let
                        folders =
                            List.Nonempty.toList lineage

                        ( model2, cmd ) =
                            model1
                                |> insertAsFolders folders
                                |> requestNeeds
                                    (NeedSubfolders (List.map .id folders))
                    in
                    ( model2
                    , cmd
                    , NoReturn
                    )

                GenericNode.IsDocument document ->
                    ( model1
                        |> updateWithModifiedDocument document
                    , Cmd.none
                    , NoReturn
                    )

                GenericNode.IsNeither ->
                    ( model1, Cmd.none, NoReturn )

        ApiResponseGenericNode nodeNumber (Err error) ->
            ( { model
                | nodeTypes =
                    Dict.insert nodeNumber (Failure error) model.nodeTypes
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseDocument documentId (Ok maybeDocument) ->
            ( { model
                | documents =
                    Dict.insert documentId (Success maybeDocument) model.documents
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseDocument documentId (Err error) ->
            ( { model
                | documents =
                    Dict.insert documentId (Failure error) model.documents
              }
            , Cmd.none
            , NoReturn
            )


insertAsFolders : List Folder -> Model -> Model
insertAsFolders listOfNewFolders model =
    { model
        | folders =
            List.foldl
                (\folder ->
                    Dict.insert folder.id (Success folder)
                )
                model.folders
                listOfNewFolders
    }


insertAsSubfolderIds : List Folder -> Model -> Model
insertAsSubfolderIds allSubfoldersOfSomeNewParents model =
    { model
        | subfolderIds =
            Dict.Extra.filterGroupBy
                .parent
                allSubfoldersOfSomeNewParents
                |> Dict.toList
                |> List.foldl
                    (\( parentId, subfolders ) ->
                        Dict.insert
                            parentId
                            (Success
                                (List.map .id subfolders)
                            )
                    )
                    model.subfolderIds
    }


insertFoldersAsNodeTypes : List Folder -> Model -> Model
insertFoldersAsNodeTypes listOfNewFolders model =
    List.foldl
        (\folder ->
            insertNodeType
                (folderIdToInt folder.id)
                (if folder.isCollection then
                    NodeIsFolder FolderIsCollection

                 else
                    NodeIsFolder FolderIsDirectory
                )
        )
        model
        listOfNewFolders


insertNodeType : Int -> NodeType -> Model -> Model
insertNodeType nodeNumber nodeType model =
    { model
        | nodeTypes =
            Dict.insert nodeNumber (Success nodeType) model.nodeTypes
    }
