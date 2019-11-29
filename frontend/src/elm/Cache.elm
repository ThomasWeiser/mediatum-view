module Cache exposing
    ( ApiData, Model, get
    , Need(..), Needs, require
    , updateWithModifiedDocument
    , ApiError, apiErrorToString
    , Msg(..), initialModel, update
    , orderingSelectionWindow
    )

{-| Manage fetching and caching of all API data.

All data needed from the API is fetched and exposed through this module.

Consuming modules declare their data needs as a value of type `Needs`.
They can read the actual data from the tables in the `Model`.

Reading the tables will result in a [`RemoteData`](/packages/krisajenkins/remotedata/6.0.1/RemoteData) value.
So the consuming modules will have to deal with the possible states a `RemoteData` can show
(`NotAsked`, `Loading`, `Failure`, `Success`).


# Cached Data

@docs ApiData, Model, get


# Declaring required data

@docs Need, Needs, require


# Modifying data locally (preliminary)

@docs updateWithModifiedDocument


# Error handling

@docs ApiError, apiErrorToString


# Elm architecture standard functions

@docs Msg, initialModel, update


# Internal functions exposed for testing only

@docs orderingSelectionWindow

-}

import Api
import Api.Queries
import Basics.Extra
import Entities.Document exposing (Document)
import Entities.DocumentResults exposing (DocumentsPage)
import Entities.Folder exposing (Folder)
import Entities.FolderCounts exposing (FolderCounts)
import Entities.GenericNode as GenericNode exposing (GenericNode)
import List.Nonempty
import Ordering exposing (Ordering)
import RemoteData exposing (RemoteData(..))
import Sort.Dict
import Types exposing (NodeType(..), Window)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Needs as Needs
import Types.Selection as Selection exposing (SelectMethod(..), Selection)
import Utils


{-| A specialization of [`RemoteData e a`](/packages/krisajenkins/remotedata/6.0.1/RemoteData#RemoteData)
where the error type `e` is defined by `ApiError`.

Any `RemoteData` used in this module uses this error type and is therefore an `ApiData`.

-}
type alias ApiData a =
    RemoteData ApiError a


{-| The type of errors that may be reported in an `ApiData.Failure`.
It's the same as `Api.Error`.
-}
type alias ApiError =
    Api.Error


{-| Represents all data for which fetching from the API has been at least started.
Consuming modules read from these tables to fulfill their data needs.

For each entity or relation in the local data schema there is a field in the `Model`.

Most fields are lookup-tables that map from a key type (representing query parameters)
to an `ApiData` type that contains (in case of a `RemoteData.Success`) the wanted content data.

-}
type alias Model =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Sort.Dict.Dict FolderId (ApiData Folder)
    , subfolderIds : Sort.Dict.Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Sort.Dict.Dict NodeId (ApiData NodeType)
    , documents : Sort.Dict.Dict DocumentId (ApiData (Maybe Document))
    , documentsPages : Sort.Dict.Dict ( Selection, Window ) (ApiData DocumentsPage)
    , folderCounts : Sort.Dict.Dict Selection (ApiData FolderCounts)
    }


{-| A data-consuming module declares its wishes for API data by means of this type.
-}
type Need
    = NeedRootFolderIds
    | NeedSubfolders (List FolderId)
    | NeedGenericNode NodeId
    | NeedDocument DocumentId
    | NeedDocumentsPage Selection Window
    | NeedFolderCounts Selection


{-| A collection of these needs.
-}
type alias Needs =
    Needs.Needs Need


{-| Describe an `ApiError` as text (aimed for debugging)
-}
apiErrorToString : ApiError -> String
apiErrorToString apiError =
    Api.errorToString apiError


{-| Initial cache model without any entry
-}
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


{-| Read an entry from a lookup-table of the `Model`.

If the given key is not yet present in the table return `RemoteData.NotAsked`.

-}
get : Sort.Dict.Dict k (ApiData v) -> k -> ApiData v
get dict key =
    Sort.Dict.get key dict
        |> Maybe.withDefault NotAsked


{-| The messages that the `update` function may process in response to an executed `Cmd`.

Currently all messages transport some API response.

-}
type Msg
    = ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))
    | ApiResponseSubfolder (List FolderId) (Api.Response (List Folder))
    | ApiResponseGenericNode NodeId (Api.Response GenericNode)
    | ApiResponseDocument DocumentId (Api.Response (Maybe Document))
    | ApiResponseDocumentsPage ( Selection, Window ) (Api.Response DocumentsPage)
    | ApiResponseFolderCounts Selection (Api.Response FolderCounts)


{-| Check which of the needed data has not yet been requested.
Submit API requests to get that data and mark the corresponding Model entries as `RemoteData.Loading`.
-}
require : Needs -> Model -> ( Model, Cmd Msg )
require needs model =
    Needs.requireNeeds
        (statusOfNeed model)
        requestNeed
        needs
        model


{-| Check the status of an atomic need.
-}
statusOfNeed : Model -> Need -> Needs.Status
statusOfNeed model need =
    case need of
        NeedRootFolderIds ->
            model.rootFolderIds
                |> Needs.statusFromRemoteData

        NeedSubfolders parentIds ->
            Needs.statusFromListOfRemoteData
                (List.map (get model.subfolderIds) parentIds)

        NeedGenericNode nodeId ->
            get model.nodeTypes nodeId
                |> Needs.statusFromRemoteData

        NeedDocument documentId ->
            get model.documents documentId
                |> Needs.statusFromRemoteData

        NeedDocumentsPage selection window ->
            get model.documentsPages ( selection, window )
                |> Needs.statusFromRemoteData

        NeedFolderCounts selection ->
            get model.folderCounts selection
                |> Needs.statusFromRemoteData


{-| Submit API requests to satisfy an atomic need.

Will be called only for needs that are known not to be in progress or fulfilled yet.

Also mark the corresponding Model entries as `RemoteData.Loading`.

-}
requestNeed : Need -> Model -> ( Model, Cmd Msg )
requestNeed need model =
    case need of
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


{-| Insert or update a document into the table `Model.documents`.
-}
updateWithModifiedDocument : Document -> Model -> Model
updateWithModifiedDocument document model =
    { model
        | documents =
            Sort.Dict.insert document.id (Success (Just document)) model.documents
    }


{-| Digest a Msg (i.e. a response from the API layer) and update the data in the `Model` accordingly.
-}
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
                                    (Needs.atomic (NeedSubfolders (List.map .id folders)))
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


{-| Ordering on the tuple type `( Selection, Window )`
-}
orderingSelectionWindow : Ordering ( Selection, Window )
orderingSelectionWindow =
    Ordering.byFieldWith Selection.orderingSelection Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Types.orderingWindow Tuple.second)
