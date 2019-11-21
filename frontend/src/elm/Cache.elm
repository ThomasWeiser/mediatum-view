module Cache exposing
    ( ApiData
    , Error(..)
    , Model
    , Msg(..)
    , Needs(..)
    , errorToString
    , get
    , initialModel
    , needsFromList
    , orderingSelectionWindow
    , require
    , update
    , updateWithModifiedDocument
    )

import Api
import Api.Queries
import Basics.Extra
import Entities.Document exposing (Document)
import Entities.Folder as Folder exposing (Folder)
import Entities.FolderCounts as FolderCounts exposing (FolderCounts)
import Entities.GenericNode as GenericNode exposing (GenericNode)
import Entities.Results exposing (DocumentsPage)
import List.Nonempty
import Ordering exposing (Ordering)
import RemoteData exposing (RemoteData(..))
import Sort.Dict
import Types exposing (NodeType(..), Window)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Selection as Selection exposing (SelectMethod(..), Selection)
import Utils


type alias ApiData a =
    RemoteData Api.Error a


type Error
    = CacheApiError Api.Error
    | CacheDataError String


type alias Model =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Sort.Dict.Dict FolderId (ApiData Folder)
    , subfolderIds : Sort.Dict.Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Sort.Dict.Dict NodeId (ApiData NodeType)
    , documents : Sort.Dict.Dict DocumentId (ApiData (Maybe Document))
    , documentsPages : Sort.Dict.Dict ( Selection, Window ) (ApiData DocumentsPage)
    , folderCounts : Sort.Dict.Dict Selection (ApiData FolderCounts)
    }


type Needs
    = NeedNothing
    | NeedAnd Needs Needs
    | NeedAndThen Needs Needs
    | NeedRootFolderIds
    | NeedSubfolders (List FolderId)
    | NeedGenericNode NodeId
    | NeedDocument DocumentId
    | NeedDocumentsPage Selection Window
    | NeedFolderCounts Selection


errorToString : Error -> String
errorToString error =
    case error of
        CacheApiError apiError ->
            Api.errorToString apiError

        CacheDataError str ->
            str


initialModel : Model
initialModel =
    { rootFolderIds = NotAsked
    , folders = Sort.Dict.empty (Utils.sorter Id.ordering)
    , subfolderIds = Sort.Dict.empty (Utils.sorter Id.ordering)
    , nodeTypes = Sort.Dict.empty (Utils.sorter Id.ordering)
    , documents = Sort.Dict.empty (Utils.sorter Id.ordering)
    , documentsPages = Sort.Dict.empty (Utils.sorter orderingSelectionWindow)
    , folderCounts = Sort.Dict.empty (Utils.sorter Selection.orderingSelection)
    }


needsFromList : List Needs -> Needs
needsFromList listOfNeeds =
    List.foldr
        (\need accu ->
            NeedAnd need accu
        )
        NeedNothing
        listOfNeeds


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

        NeedAnd needOne needTwo ->
            let
                statusOne =
                    status model needOne

                statusTwo =
                    status model needTwo
            in
            if statusOne == NotRequested || statusTwo == NotRequested then
                NotRequested

            else if statusOne == OnGoing || statusTwo == OnGoing then
                OnGoing

            else
                Fulfilled

        NeedAndThen needOne needTwo ->
            let
                statusOne =
                    status model needOne
            in
            if statusOne /= Fulfilled then
                statusOne

            else
                status model needTwo

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


require : Needs -> Model -> ( Model, Cmd Msg )
require needs model =
    if status model needs /= NotRequested then
        ( model, Cmd.none )

    else
        case needs of
            NeedNothing ->
                ( model, Cmd.none )

            NeedAnd needOne needTwo ->
                let
                    ( modelOne, cmdOne ) =
                        require needOne model

                    ( modelTwo, cmdTwo ) =
                        require needTwo modelOne
                in
                ( modelTwo
                , Cmd.batch [ cmdOne, cmdTwo ]
                )

            NeedAndThen needOne needTwo ->
                if status model needOne == Fulfilled then
                    require needTwo model

                else
                    require needOne model

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
                        (ApiResponseSubfolder parentIdsWithUnknownChildren)
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
                    (case selection.selectMethod of
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
                    (case selection.selectMethod of
                        SelectByFolderListing ->
                            Api.Queries.folderDocumentsFolderCounts
                                selection.scope
                                selection.filters

                        SelectByFullTextSearch searchTerm ftsSorting ->
                            Api.Queries.ftsFolderCounts
                                selection.scope
                                searchTerm
                                selection.filters
                    )
                )


updateWithModifiedDocument : Document -> Model -> Model
updateWithModifiedDocument document model =
    { model
        | documents =
            Sort.Dict.insert document.id (Success (Just document)) model.documents
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponseToplevelFolder (Ok listOfRootFoldersWithSubfolders) ->
            ( let
                allNewFolders =
                    listOfRootFoldersWithSubfolders
                        |> List.map (Basics.Extra.uncurry (::))
                        |> List.concat

                allRootFolderIds =
                    listOfRootFoldersWithSubfolders
                        |> List.map (Tuple.first >> .id)

                allSubfolders =
                    listOfRootFoldersWithSubfolders
                        |> List.map Tuple.second
                        |> List.concat
              in
              { model
                | rootFolderIds =
                    Success
                        (List.map (Tuple.first >> .id) listOfRootFoldersWithSubfolders)
              }
                |> insertAsFolders allNewFolders
                |> insertAsSubfolderIds allRootFolderIds allSubfolders
                |> insertFoldersAsNodeTypes allNewFolders
            , Cmd.none
            )

        ApiResponseToplevelFolder (Err error) ->
            ( { model
                | rootFolderIds = Failure error
              }
            , Cmd.none
            )

        ApiResponseSubfolder parentIds (Ok listOfSubfolders) ->
            ( model
                |> insertAsFolders listOfSubfolders
                |> insertAsSubfolderIds parentIds listOfSubfolders
                |> insertFoldersAsNodeTypes listOfSubfolders
            , Cmd.none
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
                                |> require
                                    (NeedSubfolders (List.map .id folders))
                    in
                    ( model2
                    , cmd
                    )

                GenericNode.IsDocument document ->
                    ( model1
                        |> updateWithModifiedDocument document
                    , Cmd.none
                    )

                GenericNode.IsNeither ->
                    ( model1
                    , Cmd.none
                    )

        ApiResponseGenericNode nodeId (Err error) ->
            ( { model
                | nodeTypes =
                    Sort.Dict.insert nodeId (Failure error) model.nodeTypes
              }
            , Cmd.none
            )

        ApiResponseDocument documentId result ->
            ( { model
                | documents =
                    Sort.Dict.insert documentId (RemoteData.fromResult result) model.documents
              }
            , Cmd.none
            )

        ApiResponseDocumentsPage selectionAndWindow result ->
            ( { model
                | documentsPages =
                    Sort.Dict.insert selectionAndWindow (RemoteData.fromResult result) model.documentsPages
              }
            , Cmd.none
            )

        ApiResponseFolderCounts selection result ->
            ( { model
                | folderCounts =
                    Sort.Dict.insert selection (RemoteData.fromResult result) model.folderCounts
              }
            , Cmd.none
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
        , subfolderIds =
            listOfNewFolders
                |> List.filter
                    (\folder -> folder.numSubfolder == 0)
                |> List.foldl
                    (\folder ->
                        Sort.Dict.insert folder.id (Success [])
                    )
                    model.subfolderIds
    }


insertAsSubfolderIds : List FolderId -> List Folder -> Model -> Model
insertAsSubfolderIds parentFolderIds allSubfoldersOfTheParents model =
    { model
        | subfolderIds =
            List.foldl
                (\parentFolderId ->
                    Sort.Dict.insert
                        parentFolderId
                        (Success
                            (List.filterMap
                                (\subfolder ->
                                    if subfolder.parent == Just parentFolderId then
                                        Just subfolder.id

                                    else
                                        Nothing
                                )
                                allSubfoldersOfTheParents
                            )
                        )
                )
                model.subfolderIds
                parentFolderIds
    }


insertFoldersAsNodeTypes : List Folder -> Model -> Model
insertFoldersAsNodeTypes listOfNewFolders model =
    List.foldl
        (\folder ->
            insertNodeType
                (folder.id |> Id.asNodeId)
                (NodeIsFolder folder.display)
        )
        model
        listOfNewFolders


insertNodeType : NodeId -> NodeType -> Model -> Model
insertNodeType nodeId nodeType model =
    { model
        | nodeTypes =
            Sort.Dict.insert nodeId (Success nodeType) model.nodeTypes
    }


orderingSelectionWindow : Ordering ( Selection, Window )
orderingSelectionWindow =
    Ordering.byFieldWith Selection.orderingSelection Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Types.orderingWindow Tuple.second)