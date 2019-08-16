module Data.Cache exposing
    ( ApiData
    , Model
    , Msg(..)
    , Needs(..)
    , Return(..)
    , get
    , initialModel
    , requestNeeds
    , update
    , updateWithModifiedDocument
    )

import Api
import Api.Queries
import Basics.Extra
import Data.Ordering exposing (..)
import Data.Types exposing (..)
import Dict
import Dict.Extra
import GenericNode exposing (GenericNode)
import List.Extra
import List.Nonempty exposing (Nonempty)
import RemoteData exposing (RemoteData(..))
import Sort
import Sort.Dict


type alias ApiData a =
    RemoteData Api.Error a


type Return
    = NoReturn
    | GotRootFolders


type alias Model =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Sort.Dict.Dict FolderId (ApiData Folder)
    , subfolderIds : Sort.Dict.Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Sort.Dict.Dict NodeId (ApiData NodeType)
    , documents : Sort.Dict.Dict DocumentId (ApiData (Maybe Document))
    , documentsPages : Sort.Dict.Dict ( Selection, Window ) (ApiData DocumentsPage)
    , folderCounts : Sort.Dict.Dict Selection (ApiData (Sort.Dict.Dict FolderId Int))
    }


type Needs
    = NeedNothing
    | NeedListOfNeeds (List Needs)
    | NeedRootFolderIds
    | NeedSubfolders (List FolderId)
    | NeedGenericNode NodeId
    | NeedDocument DocumentId
    | NeedDocumentsPage Selection Window
    | NeedFolderCounts Selection


initialModel : Model
initialModel =
    { rootFolderIds = NotAsked
    , folders = Sort.Dict.empty (sorter orderingFolderId)
    , subfolderIds = Sort.Dict.empty (sorter orderingFolderId)
    , nodeTypes = Sort.Dict.empty (sorter orderingNodeId)
    , documents = Sort.Dict.empty (sorter orderingDocumentId)
    , documentsPages = Sort.Dict.empty (sorter orderingSelectionWindow)
    , folderCounts = Sort.Dict.empty (sorter orderingSelection)
    }


get : Sort.Dict.Dict k (ApiData v) -> k -> ApiData v
get dict key =
    Sort.Dict.get key dict
        |> Maybe.withDefault NotAsked


type Msg
    = ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))
    | ApiResponseSubfolder (List FolderId) (Api.Response (List Folder))
    | ApiResponseGenericNode NodeId (Api.Response GenericNode)
    | ApiResponseDocument DocumentId (Api.Response (Maybe Document))
    | ApiResponseDocumentsPage ( Selection, Window ) (Api.Response DocumentsPage)
    | ApiResponseFolderCounts Selection (Api.Response FolderCounts)


type Status
    = NotRequested
    | Fulfilled
    | OnGoing


statusFromRemoteData : RemoteData e a -> Status
statusFromRemoteData remoteData =
    case remoteData of
        NotAsked ->
            NotRequested

        Loading ->
            OnGoing

        Failure _ ->
            OnGoing

        Success _ ->
            Fulfilled


status : Model -> Needs -> Status
status model needs =
    case needs of
        NeedNothing ->
            Fulfilled

        NeedListOfNeeds listOfNeeds ->
            let
                listOfStatus =
                    List.map (status model) listOfNeeds
            in
            if List.member OnGoing listOfStatus then
                OnGoing

            else if List.member NotRequested listOfStatus then
                NotRequested

            else
                Fulfilled

        NeedRootFolderIds ->
            model.rootFolderIds
                |> statusFromRemoteData

        NeedSubfolders parentIds ->
            let
                listOfRemoteData =
                    List.map (get model.subfolderIds) parentIds
            in
            if List.any RemoteData.isNotAsked listOfRemoteData then
                NotRequested

            else if List.all RemoteData.isSuccess listOfRemoteData then
                Fulfilled

            else
                OnGoing

        NeedGenericNode nodeId ->
            get model.nodeTypes nodeId
                |> statusFromRemoteData

        NeedDocument documentId ->
            get model.documents documentId
                |> statusFromRemoteData

        NeedDocumentsPage selection window ->
            get model.documentsPages ( selection, window )
                |> statusFromRemoteData

        NeedFolderCounts selection ->
            get model.folderCounts selection
                |> statusFromRemoteData


requestNeeds : Needs -> Model -> ( Model, Cmd Msg )
requestNeeds needs model =
    if status model needs /= NotRequested then
        ( model, Cmd.none )

    else
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
                ( { model
                    | rootFolderIds = Loading
                  }
                , Api.sendQueryRequest
                    ApiResponseToplevelFolder
                    Api.Queries.toplevelFolder
                )

            NeedSubfolders parentIds ->
                let
                    parentIdsWithUnknownChildren =
                        List.filter
                            (\parentId ->
                                get model.subfolderIds parentId == NotAsked
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
                                    Sort.Dict.insert parentId Loading subfolderIds
                                )
                                model.subfolderIds
                                parentIdsWithUnknownChildren
                      }
                    , Api.sendQueryRequest
                        (ApiResponseSubfolder parentIds)
                        (Api.Queries.subfolder parentIdsWithUnknownChildren)
                    )

            NeedGenericNode nodeId ->
                ( { model
                    | nodeTypes =
                        Sort.Dict.insert nodeId Loading model.nodeTypes
                  }
                , Api.sendQueryRequest
                    (ApiResponseGenericNode nodeId)
                    (Api.Queries.genericNode nodeId)
                )

            NeedDocument documentId ->
                ( { model
                    | documents =
                        Sort.Dict.insert documentId Loading model.documents
                  }
                , Api.sendQueryRequest
                    (ApiResponseDocument documentId)
                    (Api.Queries.documentDetails documentId)
                )

            NeedDocumentsPage selection window ->
                ( { model
                    | documentsPages =
                        Sort.Dict.insert ( selection, window ) Loading model.documentsPages
                  }
                , Api.sendQueryRequest
                    (ApiResponseDocumentsPage ( selection, window ))
                    (case selection.searchMethod of
                        SelectByFolderListing ->
                            Api.Queries.folderDocumentsPage
                                window
                                selection.scope
                                selection.filters

                        SelectByFullTextSearch searchTerm ftsSorting ->
                            Api.Queries.ftsPage
                                window
                                selection.scope
                                searchTerm
                                ftsSorting
                                selection.filters
                    )
                )

            NeedFolderCounts selection ->
                ( { model
                    | folderCounts =
                        Sort.Dict.insert selection Loading model.folderCounts
                  }
                , Api.sendQueryRequest
                    (ApiResponseFolderCounts selection)
                    (case selection.searchMethod of
                        SelectByFolderListing ->
                            Api.Queries.folderDocumentsFolderCounts
                                selection.scope
                                selection.filters

                        SelectByFullTextSearch searchTerm ftsSorting ->
                            Api.Queries.ftsFolderCounts
                                selection.scope
                                searchTerm
                                ftsSorting
                                selection.filters
                    )
                )


updateWithModifiedDocument : Document -> Model -> Model
updateWithModifiedDocument document model =
    { model
        | documents =
            Sort.Dict.insert document.id (Success (Just document)) model.documents
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
                            Sort.Dict.insert parentId (Failure error) subfolderIds
                        )
                        model.subfolderIds
                        parentIds
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseGenericNode nodeId (Ok genericNode) ->
            let
                model1 =
                    model
                        |> insertNodeType nodeId (GenericNode.toNodeType genericNode)
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

        ApiResponseGenericNode nodeId (Err error) ->
            ( { model
                | nodeTypes =
                    Sort.Dict.insert nodeId (Failure error) model.nodeTypes
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseDocument documentId (Ok maybeDocument) ->
            ( { model
                | documents =
                    Sort.Dict.insert documentId (Success maybeDocument) model.documents
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseDocument documentId (Err error) ->
            ( { model
                | documents =
                    Sort.Dict.insert documentId (Failure error) model.documents
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseDocumentsPage selectionAndWindow (Ok documentsPage) ->
            ( { model
                | documentsPages =
                    Sort.Dict.insert selectionAndWindow (Success documentsPage) model.documentsPages
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseDocumentsPage selectionAndWindow (Err error) ->
            ( { model
                | documentsPages =
                    Sort.Dict.insert selectionAndWindow (Failure error) model.documentsPages
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseFolderCounts selection (Ok folderCounts) ->
            ( { model
                | folderCounts =
                    Sort.Dict.insert selection (Success folderCounts) model.folderCounts
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseFolderCounts selection (Err error) ->
            ( { model
                | folderCounts =
                    Sort.Dict.insert selection (Failure error) model.folderCounts
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
                    Sort.Dict.insert folder.id (Success folder)
                )
                model.folders
                listOfNewFolders
    }


insertAsSubfolderIds : List Folder -> Model -> Model
insertAsSubfolderIds allSubfoldersOfSomeNewParents model =
    { model
        | subfolderIds =
            Dict.Extra.filterGroupBy
                (.parent >> Maybe.map folderIdToInt)
                allSubfoldersOfSomeNewParents
                |> Dict.toList
                |> List.foldl
                    (\( parentIdAsInt, subfolders ) ->
                        Sort.Dict.insert
                            (folderIdFromInt parentIdAsInt)
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
                (folder.id |> folderIdToInt |> nodeIdFromInt)
                (NodeIsFolder folder.type_)
        )
        model
        listOfNewFolders


insertNodeType : NodeId -> NodeType -> Model -> Model
insertNodeType nodeId nodeType model =
    { model
        | nodeTypes =
            Sort.Dict.insert nodeId (Success nodeType) model.nodeTypes
    }
