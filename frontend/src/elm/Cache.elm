module Cache exposing
    ( Cache, ApiData, get
    , Need(..), Needs, targetNeeds
    , updateWithModifiedDocument
    , ApiError, apiErrorToString
    , Msg(..), init, update
    , orderingSelectionWindow, orderingSelectionFacet
    )

{-| Manage fetching and caching of all API data.

All data needed from the API is fetched and exposed through this module.

Consuming modules declare their data needs as a value of type [`Needs`](#Needs).
They can read the actual data from the tables in the [`Cache`](#Cache).

Reading the tables will result in a [`RemoteData`](/packages/krisajenkins/remotedata/6.0.1/RemoteData) value.
So the consuming modules will have to deal with the possible states a `RemoteData` can show
(`NotAsked`, `Loading`, `Failure`, `Success`).


# Cached Data

@docs Cache, ApiData, get


# Declaring required data

@docs Need, Needs, targetNeeds


# Modifying data locally (preliminary)

@docs updateWithModifiedDocument


# Error handling

@docs ApiError, apiErrorToString


# Elm architecture standard functions

@docs Msg, init, update


# Internal functions exposed for testing only

@docs orderingSelectionWindow, orderingSelectionFacet

-}

import Api
import Api.Queries
import Basics.Extra
import Config
import Entities.Document exposing (Document)
import Entities.DocumentResults exposing (DocumentsPage)
import Entities.Folder exposing (Folder)
import Entities.FolderCounts exposing (FolderCounts)
import Entities.GenericNode as GenericNode exposing (GenericNode)
import Entities.Residence as Residence exposing (Residence)
import List.Nonempty
import Ordering exposing (Ordering)
import RemoteData exposing (RemoteData(..))
import Sort.Dict
import Types exposing (DocumentIdFromSearch, NodeType(..), Window)
import Types.Facet exposing (FacetValues)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Needs as Needs
import Types.SearchTerm exposing (SearchTerm)
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


{-| Represents all known data (in whatever state it may be: `Loading`, `Failure` or `Success`).

Consuming modules read from these tables to fulfill their data needs.

For each entity or relation in the local data schema there is a field in the `Cache` record.

Most fields are lookup-tables that map from a key type (representing query parameters)
to an `ApiData` type that contains (in case of a `RemoteData.Success`) the wanted content data.

-}
type alias Cache =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Sort.Dict.Dict FolderId (ApiData Folder)
    , subfolderIds : Sort.Dict.Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Sort.Dict.Dict NodeId (ApiData NodeType)
    , documents : Sort.Dict.Dict DocumentIdFromSearch (ApiData (Maybe Document))
    , residence : Sort.Dict.Dict DocumentId (ApiData Residence)
    , documentsPages : Sort.Dict.Dict ( Selection, Window ) (ApiData DocumentsPage)
    , folderCounts : Sort.Dict.Dict Selection (ApiData FolderCounts)
    , facetsValues : Sort.Dict.Dict ( Selection, String ) (ApiData FacetValues)
    }


{-| A data-consuming module declares its wishes for API data by means of this type.
-}
type Need
    = NeedRootFolderIds
    | NeedFolders (List FolderId)
    | NeedSubfolders (List FolderId)
    | NeedGenericNode NodeId
    | NeedDocumentFromSearch DocumentIdFromSearch
    | NeedDocumentsPage Selection Window
    | NeedFolderCounts Selection
    | NeedFacet Selection String


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
init : Cache
init =
    { rootFolderIds = NotAsked
    , folders = Sort.Dict.empty (Utils.sorter Id.ordering)
    , subfolderIds = Sort.Dict.empty (Utils.sorter Id.ordering)
    , nodeTypes = Sort.Dict.empty (Utils.sorter Id.ordering)
    , documents = Sort.Dict.empty (Utils.sorter Types.orderingDocumentIdFromSearch)
    , residence = Sort.Dict.empty (Utils.sorter Id.ordering)
    , documentsPages = Sort.Dict.empty (Utils.sorter orderingSelectionWindow)
    , folderCounts = Sort.Dict.empty (Utils.sorter Selection.orderingSelectionModuloSorting)
    , facetsValues = Sort.Dict.empty (Utils.sorter orderingSelectionFacet)
    }


{-| Read an entry from a lookup-table of the cache.

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
    = ApiResponseToplevelFolders (Api.Response (List ( Folder, List Folder )))
    | ApiResponseFolders (List FolderId) (Api.Response (List Folder))
    | ApiResponseSubfolders (List FolderId) (Api.Response (List Folder))
    | ApiResponseGenericNode NodeId (Api.Response GenericNode)
    | ApiResponseDocumentFromSearch DocumentIdFromSearch (Api.Response (Maybe ( Document, Residence )))
    | ApiResponseDocumentsPage ( Selection, Window ) (Api.Response DocumentsPage)
    | ApiResponseFolderCounts Selection (Api.Response FolderCounts)
    | ApiResponseFacet ( Selection, String ) (Api.Response FacetValues)


{-| Check which of the needed data has not yet been requested.
Submit API requests to get that data and mark the corresponding cache entries as `RemoteData.Loading`.
-}
targetNeeds : Needs -> Cache -> ( Cache, Cmd Msg )
targetNeeds needs cache =
    Needs.target
        (statusOfNeed cache)
        requestNeed
        needs
        cache


{-| Check the status of an atomic need.
-}
statusOfNeed : Cache -> Need -> Needs.Status
statusOfNeed cache need =
    case need of
        NeedRootFolderIds ->
            cache.rootFolderIds
                |> Needs.statusFromRemoteData

        NeedFolders folderIds ->
            Needs.statusFromListOfRemoteData
                (List.map (get cache.folders) folderIds)

        NeedSubfolders parentIds ->
            Needs.statusFromListOfRemoteData
                (List.map (get cache.subfolderIds) parentIds)

        NeedGenericNode nodeId ->
            get cache.nodeTypes nodeId
                |> Needs.statusFromRemoteData

        NeedDocumentFromSearch documentId ->
            get cache.documents documentId
                |> Needs.statusFromRemoteData

        NeedDocumentsPage selection window ->
            get cache.documentsPages ( selection, window )
                |> Needs.statusFromRemoteData

        NeedFolderCounts selection ->
            get cache.folderCounts selection
                |> Needs.statusFromRemoteData

        NeedFacet selection key ->
            get cache.facetsValues ( selection, key )
                |> Needs.statusFromRemoteData


{-| Submit API requests to satisfy an atomic need.

Will be called only for needs that are known not to be in progress or fulfilled yet.

Also mark the corresponding cache entries as `RemoteData.Loading`.

-}
requestNeed : Need -> Cache -> ( Cache, Cmd Msg )
requestNeed need cache =
    case need of
        NeedRootFolderIds ->
            ( { cache
                | rootFolderIds = Loading
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedRootFolderIds")
                ApiResponseToplevelFolders
                Api.Queries.toplevelFolders
            )

        NeedFolders folderIds ->
            let
                unknownFolderIds =
                    List.filter
                        (\folderId ->
                            get cache.folders folderId == NotAsked
                        )
                        folderIds
            in
            if List.isEmpty unknownFolderIds then
                ( cache, Cmd.none )

            else
                ( { cache
                    | folders =
                        List.foldl
                            (\folderId folders ->
                                Sort.Dict.insert folderId Loading folders
                            )
                            cache.folders
                            unknownFolderIds
                  }
                , Api.sendQueryRequest
                    (Api.withOperationName "NeedFolders")
                    (ApiResponseFolders unknownFolderIds)
                    (Api.Queries.folders unknownFolderIds)
                )

        NeedSubfolders parentIds ->
            let
                parentIdsWithUnknownChildren =
                    List.filter
                        (\parentId ->
                            get cache.subfolderIds parentId == NotAsked
                        )
                        parentIds
            in
            if List.isEmpty parentIdsWithUnknownChildren then
                ( cache, Cmd.none )

            else
                ( { cache
                    | subfolderIds =
                        List.foldl
                            (\parentId subfolderIds ->
                                Sort.Dict.insert parentId Loading subfolderIds
                            )
                            cache.subfolderIds
                            parentIdsWithUnknownChildren
                  }
                , Api.sendQueryRequest
                    (Api.withOperationName "NeedSubfolders")
                    (ApiResponseSubfolders parentIdsWithUnknownChildren)
                    (Api.Queries.subfolders parentIdsWithUnknownChildren)
                )

        NeedGenericNode nodeId ->
            ( { cache
                | nodeTypes =
                    Sort.Dict.insert nodeId Loading cache.nodeTypes
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedGenericNode")
                (ApiResponseGenericNode nodeId)
                (Api.Queries.genericNode nodeId)
            )

        NeedDocumentFromSearch documentIdFromSearch ->
            ( { cache
                | documents =
                    Sort.Dict.insert documentIdFromSearch Loading cache.documents
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedDocument")
                (ApiResponseDocumentFromSearch documentIdFromSearch)
                (Api.Queries.documentDetails documentIdFromSearch.id)
            )

        NeedDocumentsPage selection window ->
            ( { cache
                | documentsPages =
                    Sort.Dict.insert ( selection, window ) Loading cache.documentsPages
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedDocumentsPage")
                (ApiResponseDocumentsPage ( selection, window ))
                (Api.Queries.selectionDocumentsPage window selection)
            )

        NeedFolderCounts selection ->
            ( { cache
                | folderCounts =
                    Sort.Dict.insert selection Loading cache.folderCounts
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedFolderCounts")
                (ApiResponseFolderCounts selection)
                (Api.Queries.selectionFolderCounts selection)
            )

        NeedFacet selection key ->
            ( { cache
                | facetsValues =
                    Sort.Dict.insert ( selection, key ) Loading cache.facetsValues
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedFacet")
                (ApiResponseFacet ( selection, key ))
                (Api.Queries.selectionFacetByKey selection key Config.facetValuesToQuery)
            )


{-| Insert or update a document into the table `Cache.documents`.
-}
updateWithModifiedDocument : Document -> Cache -> Cache
updateWithModifiedDocument document cache =
    { cache
        | documents =
            Sort.Dict.insert
                (DocumentIdFromSearch document.id Nothing)
                (Success (Just document))
                cache.documents
    }


{-| Digest a Msg (i.e. a response from the API layer) and update the data in the cache accordingly.
-}
update : Msg -> Cache -> ( Cache, Cmd Msg )
update msg cache =
    case msg of
        ApiResponseToplevelFolders (Ok listOfRootFoldersWithSubfolders) ->
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
              { cache
                | rootFolderIds =
                    Success
                        (List.map (Tuple.first >> .id) listOfRootFoldersWithSubfolders)
              }
                |> insertAsFolders allNewFolders
                |> insertAsSubfolderIds allRootFolderIds allSubfolders
                |> insertFoldersAsNodeTypes allNewFolders
            , Cmd.none
            )

        ApiResponseToplevelFolders (Err error) ->
            ( { cache
                | rootFolderIds = Failure error
              }
            , Cmd.none
            )

        ApiResponseFolders folderIds (Ok listOfFolders) ->
            ( cache
                |> insertAsFolders listOfFolders
                |> insertFoldersAsNodeTypes listOfFolders
            , Cmd.none
            )

        ApiResponseFolders folderIds (Err error) ->
            ( { cache
                | folders =
                    List.foldl
                        (\folderId folders ->
                            Sort.Dict.insert folderId (Failure error) folders
                        )
                        cache.folders
                        folderIds
              }
            , Cmd.none
            )

        ApiResponseSubfolders parentIds (Ok listOfSubfolders) ->
            ( cache
                |> insertAsFolders listOfSubfolders
                |> insertAsSubfolderIds parentIds listOfSubfolders
                |> insertFoldersAsNodeTypes listOfSubfolders
            , Cmd.none
            )

        ApiResponseSubfolders parentIds (Err error) ->
            ( { cache
                | subfolderIds =
                    List.foldl
                        (\parentId subfolderIds ->
                            Sort.Dict.insert parentId (Failure error) subfolderIds
                        )
                        cache.subfolderIds
                        parentIds
              }
            , Cmd.none
            )

        ApiResponseGenericNode nodeId (Ok genericNode) ->
            let
                cache1 =
                    cache
                        |> insertNodeType nodeId (GenericNode.toNodeType genericNode)
            in
            case genericNode of
                GenericNode.IsFolder lineage ->
                    let
                        folders =
                            List.Nonempty.toList lineage
                    in
                    cache1
                        |> insertAsFolders folders
                        |> targetNeeds
                            (Needs.atomic (NeedSubfolders (List.map .id folders)))

                GenericNode.IsDocument ( document, residence ) ->
                    updateWithDocumentAndResidence
                        (DocumentIdFromSearch document.id Nothing)
                        (Just ( document, residence ))
                        cache1

                GenericNode.IsNeither ->
                    ( cache1
                    , Cmd.none
                    )

        ApiResponseGenericNode nodeId (Err error) ->
            ( { cache
                | nodeTypes =
                    Sort.Dict.insert nodeId (Failure error) cache.nodeTypes
              }
            , Cmd.none
            )

        ApiResponseDocumentFromSearch documentIdFromSearch (Ok maybeDocumentAndResidence) ->
            updateWithDocumentAndResidence
                documentIdFromSearch
                maybeDocumentAndResidence
                cache

        ApiResponseDocumentFromSearch documentIdFromSearch (Err error) ->
            ( { cache
                | documents =
                    Sort.Dict.insert documentIdFromSearch (Failure error) cache.documents
              }
            , Cmd.none
            )

        ApiResponseDocumentsPage selectionAndWindow result ->
            ( { cache
                | documentsPages =
                    Sort.Dict.insert selectionAndWindow (RemoteData.fromResult result) cache.documentsPages
              }
            , Cmd.none
            )

        ApiResponseFolderCounts selection result ->
            ( { cache
                | folderCounts =
                    Sort.Dict.insert selection (RemoteData.fromResult result) cache.folderCounts
              }
            , Cmd.none
            )

        ApiResponseFacet selectionAndKey result ->
            ( { cache
                | facetsValues =
                    Sort.Dict.insert selectionAndKey (RemoteData.fromResult result) cache.facetsValues
              }
            , Cmd.none
            )


updateWithDocumentAndResidence : DocumentIdFromSearch -> Maybe ( Document, Residence ) -> Cache -> ( Cache, Cmd Msg )
updateWithDocumentAndResidence documentIdFromSearch maybeDocumentAndResidence cache =
    let
        cache1 =
            case maybeDocumentAndResidence of
                Just ( document, residence ) ->
                    { cache
                        | documents =
                            Sort.Dict.insert
                                documentIdFromSearch
                                (Success (Just document))
                                cache.documents
                        , residence =
                            Sort.Dict.insert
                                documentIdFromSearch.id
                                (Success residence)
                                cache.residence
                    }

                Nothing ->
                    { cache
                        | documents =
                            Sort.Dict.insert
                                documentIdFromSearch
                                (Success Nothing)
                                cache.documents
                    }
    in
    targetNeeds
        (case maybeDocumentAndResidence of
            Just ( _, residence ) ->
                Needs.atomic
                    (NeedFolders
                        (Residence.toList residence)
                    )

            Nothing ->
                Needs.none
        )
        cache1


insertAsFolders : List Folder -> Cache -> Cache
insertAsFolders listOfNewFolders cache =
    { cache
        | folders =
            List.foldl
                (\folder ->
                    Sort.Dict.insert folder.id (Success folder)
                )
                cache.folders
                listOfNewFolders
        , subfolderIds =
            listOfNewFolders
                |> List.filter
                    (\folder -> folder.numSubfolder == 0)
                |> List.foldl
                    (\folder ->
                        Sort.Dict.insert folder.id (Success [])
                    )
                    cache.subfolderIds
    }


insertAsSubfolderIds : List FolderId -> List Folder -> Cache -> Cache
insertAsSubfolderIds parentFolderIds allSubfoldersOfTheParents cache =
    { cache
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
                cache.subfolderIds
                parentFolderIds
    }


insertFoldersAsNodeTypes : List Folder -> Cache -> Cache
insertFoldersAsNodeTypes listOfNewFolders cache =
    List.foldl
        (\folder ->
            insertNodeType
                (folder.id |> Id.asNodeId)
                (NodeIsFolder folder.display)
        )
        cache
        listOfNewFolders


insertNodeType : NodeId -> NodeType -> Cache -> Cache
insertNodeType nodeId nodeType cache =
    { cache
        | nodeTypes =
            Sort.Dict.insert nodeId (Success nodeType) cache.nodeTypes
    }


{-| Ordering on the tuple type `( Selection, Window )`
-}
orderingSelectionWindow : Ordering ( Selection, Window )
orderingSelectionWindow =
    Ordering.byFieldWith Selection.orderingSelection Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Types.orderingWindow Tuple.second)


{-| Ordering on the tuple type `( Selection, String )`
-}
orderingSelectionFacet : Ordering ( Selection, String )
orderingSelectionFacet =
    Ordering.byFieldWith Selection.orderingSelectionModuloSorting Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Ordering.natural Tuple.second)
