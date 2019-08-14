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
    , nodeTypes : Sort.Dict.Dict Int (ApiData NodeType)
    , documents : Sort.Dict.Dict DocumentId (ApiData (Maybe Document))
    , documentsPages : Sort.Dict.Dict ( Selection, Window ) (ApiData DocumentsPage)
    , folderCounts : Sort.Dict.Dict Selection (ApiData (Dict.Dict FolderId Int))
    }


type Needs
    = NeedNothing
    | NeedListOfNeeds (List Needs)
    | NeedRootFolderIds
    | NeedSubfolders (List FolderId)
    | NeedGenericNode Int
    | NeedDocument DocumentId
    | NeedDocumentsPage Selection Window
    | NeedFolderCounts Selection


initialModel : Model
initialModel =
    { rootFolderIds = NotAsked
    , folders = Sort.Dict.empty (sorter orderingFolderId)
    , subfolderIds = Sort.Dict.empty (sorter orderingFolderId)
    , nodeTypes = Sort.Dict.empty Sort.increasing
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
    | ApiResponseGenericNode Int (Api.Response GenericNode)
    | ApiResponseDocument DocumentId (Api.Response (Maybe Document))
    | ApiResponseDocumentsPage ( Selection, Window ) (Api.Response DocumentsPage)
    | ApiResponseFolderCounts Selection (Api.Response FolderCounts)


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

        NeedGenericNode nodeNumber ->
            case get model.nodeTypes nodeNumber of
                NotAsked ->
                    ( { model
                        | nodeTypes =
                            Sort.Dict.insert nodeNumber Loading model.nodeTypes
                      }
                    , Api.sendQueryRequest
                        (ApiResponseGenericNode nodeNumber)
                        (Api.Queries.genericNode nodeNumber)
                    )

                _ ->
                    ( model, Cmd.none )

        NeedDocument documentId ->
            case get model.documents documentId of
                NotAsked ->
                    ( { model
                        | documents =
                            Sort.Dict.insert documentId Loading model.documents
                      }
                    , Api.sendQueryRequest
                        (ApiResponseDocument documentId)
                        (Api.Queries.documentDetails documentId)
                    )

                _ ->
                    ( model, Cmd.none )

        NeedDocumentsPage selection window ->
            case get model.documentsPages ( selection, window ) of
                NotAsked ->
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

                _ ->
                    ( model, Cmd.none )

        NeedFolderCounts selection ->
            case get model.folderCounts selection of
                NotAsked ->
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

                _ ->
                    ( model, Cmd.none )


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
                    Sort.Dict.insert nodeNumber (Failure error) model.nodeTypes
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
                .parent
                allSubfoldersOfSomeNewParents
                |> Dict.toList
                |> List.foldl
                    (\( parentId, subfolders ) ->
                        Sort.Dict.insert
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
            Sort.Dict.insert nodeNumber (Success nodeType) model.nodeTypes
    }
