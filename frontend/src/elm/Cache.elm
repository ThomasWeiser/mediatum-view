module Cache exposing
    ( Cache, get
    , Need(..), Needs, targetNeeds
    , updateWithModifiedDocument
    , Msg(..), init, update
    , orderingSelectionWindow, orderingMaskSelectionWindow, orderingSelectionFacets, orderingMaskDocumentIdFromSearch
    )

{-| Manage fetching and caching of all API data.

All data needed from the API is fetched and exposed through this module.

Consuming modules declare their data needs as a value of type [`Needs`](#Needs).
They can read the actual data from the tables in the [`Cache`](#Cache).

Reading the tables will result in a [`RemoteData`](/packages/krisajenkins/remotedata/6.0.1/RemoteData) value.
So the consuming modules will have to deal with the possible states a `RemoteData` can show
(`NotAsked`, `Loading`, `Failure`, `Success`).


# Cached Data

@docs Cache, get


# Declaring required data

@docs Need, Needs, targetNeeds


# Modifying data locally (preliminary)

@docs updateWithModifiedDocument


# Elm architecture standard functions

@docs Msg, init, update


# Internal functions exposed for testing only

@docs orderingSelectionWindow, orderingMaskSelectionWindow, orderingSelectionFacets, orderingMaskDocumentIdFromSearch

-}

import Api
import Api.Queries
import Entities.Document exposing (Document)
import Entities.DocumentResults exposing (DocumentsPage)
import Entities.Folder exposing (Folder)
import Entities.FolderCounts exposing (FolderCounts)
import Entities.GenericNode as GenericNode exposing (GenericNode)
import Entities.PageSequence as PageSequence exposing (PageSequence)
import Entities.Residence as Residence exposing (Residence)
import List.Nonempty
import Ordering exposing (Ordering)
import RemoteData exposing (RemoteData(..))
import Sort.Dict
import Types exposing (DocumentIdFromSearch, NodeType(..), Window)
import Types.ApiData exposing (ApiData)
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config exposing (Config)
import Types.FacetValue exposing (FacetsValues)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Needs as Needs
import Types.Selection as Selection exposing (Selection)
import Utils
import Utils.List


{-| Represents all known data (in whatever state it may be: `Loading`, `Failure` or `Success`).

Consuming modules read from these tables to fulfill their data needs.

For each entity or relation in the local data schema there is a field in the `Cache` record.

All fields are lookup-tables that map from a key type (representing query parameters)
to an `ApiData` type that contains (in case of a `RemoteData.Success`) the wanted content data.

-}
type alias Cache =
    { folders : Sort.Dict.Dict FolderId (ApiData Folder)
    , subfolderIds : Sort.Dict.Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Sort.Dict.Dict NodeId (ApiData NodeType)
    , documents : Sort.Dict.Dict ( String, DocumentIdFromSearch ) (ApiData Document)
    , residence : Sort.Dict.Dict DocumentId (ApiData Residence)
    , documentsPages : Sort.Dict.Dict ( String, Selection, Window ) (ApiData DocumentsPage)
    , folderCounts : Sort.Dict.Dict Selection (ApiData FolderCounts)
    , facetsValues : Sort.Dict.Dict ( Selection, List Aspect ) (ApiData FacetsValues)
    }


{-| A data-consuming module declares its wishes for API data by means of this type.
-}
type Need
    = NeedFolders (List FolderId)
    | NeedSubfolders (List FolderId)
    | NeedGenericNode String NodeId
    | NeedDocumentFromSearch String DocumentIdFromSearch
    | NeedDocumentsPage String Selection Window
    | NeedFolderCounts Selection
    | NeedFacets Selection (List Aspect)


{-| A collection of these needs.
-}
type alias Needs =
    Needs.Needs Need


{-| Initial cache model without any entry
-}
init : Cache
init =
    { folders = Sort.Dict.empty (Utils.sorter Id.ordering)
    , subfolderIds = Sort.Dict.empty (Utils.sorter Id.ordering)
    , nodeTypes = Sort.Dict.empty (Utils.sorter Id.ordering)
    , documents = Sort.Dict.empty (Utils.sorter orderingMaskDocumentIdFromSearch)
    , residence = Sort.Dict.empty (Utils.sorter Id.ordering)
    , documentsPages = Sort.Dict.empty (Utils.sorter orderingMaskSelectionWindow)
    , folderCounts = Sort.Dict.empty (Utils.sorter Selection.orderingSelectionModuloSorting)
    , facetsValues = Sort.Dict.empty (Utils.sorter orderingSelectionFacets)
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
    = ApiResponseFolders (List FolderId) (Api.Response (List Folder))
    | ApiResponseSubfolders (List FolderId) (Api.Response (List Folder))
    | ApiResponseGenericNode String NodeId (Api.Response GenericNode)
    | ApiResponseDocumentFromSearch String DocumentIdFromSearch (Api.Response (Maybe ( Document, Maybe Residence )))
    | ApiResponseDocumentsPage ( String, Selection, Window ) (Api.Response DocumentsPage)
    | ApiResponseFolderCounts Selection (Api.Response FolderCounts)
    | ApiResponseFacets ( Selection, List Aspect ) (Api.Response FacetsValues)


{-| Check which of the needed data has not yet been requested.
Submit API requests to get that data and mark the corresponding cache entries as `RemoteData.Loading`.
-}
targetNeeds : Config -> Needs -> Cache -> ( Cache, Cmd Msg )
targetNeeds config needs cache =
    Needs.target
        (statusOfNeed config cache)
        (requestNeed config)
        needs
        cache


{-| Check the status of an atomic need.
-}
statusOfNeed : Config -> Cache -> Need -> Needs.Status
statusOfNeed config cache need =
    case need of
        NeedFolders folderIds ->
            Needs.statusFromListOfRemoteData
                (List.map (get cache.folders) folderIds)

        NeedSubfolders parentIds ->
            Needs.statusFromListOfRemoteData
                (List.map (get cache.subfolderIds) parentIds)

        NeedGenericNode maskName nodeId ->
            get cache.nodeTypes nodeId
                |> Needs.statusFromRemoteData

        NeedDocumentFromSearch maskName documentIdFromSearch ->
            get cache.documents ( maskName, documentIdFromSearch )
                |> Needs.statusFromRemoteData

        NeedDocumentsPage maskName selection window ->
            get cache.documentsPages ( maskName, selection, window )
                |> Needs.statusFromRemoteData

        NeedFolderCounts selection ->
            get cache.folderCounts selection
                |> Needs.statusFromRemoteData

        NeedFacets selection aspects ->
            get cache.facetsValues ( selection, aspects )
                |> Needs.statusFromRemoteData


{-| Submit API requests to satisfy an atomic need.

Will be called only for needs that are known not to be in progress or fulfilled yet.

Also mark the corresponding cache entries as `RemoteData.Loading`.

-}
requestNeed : Config -> Need -> Cache -> ( Cache, Cmd Msg )
requestNeed config need cache =
    case need of
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

        NeedGenericNode maskName nodeId ->
            ( { cache
                | nodeTypes =
                    Sort.Dict.insert nodeId Loading cache.nodeTypes
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedGenericNode")
                (ApiResponseGenericNode maskName nodeId)
                (Api.Queries.genericNode maskName nodeId)
            )

        NeedDocumentFromSearch maskName documentIdFromSearch ->
            let
                needResidence =
                    Needs.statusFromRemoteData
                        (get cache.residence documentIdFromSearch.id)
                        == Needs.NotRequested
            in
            ( { cache
                | documents =
                    Sort.Dict.insert ( maskName, documentIdFromSearch ) Loading cache.documents
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedDocumentFromSearch")
                (ApiResponseDocumentFromSearch maskName documentIdFromSearch)
                (Api.Queries.documentDetails maskName documentIdFromSearch needResidence)
            )

        NeedDocumentsPage maskName selection window ->
            ( { cache
                | documentsPages =
                    Sort.Dict.insert ( maskName, selection, window ) Loading cache.documentsPages
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedDocumentsPage")
                (ApiResponseDocumentsPage ( maskName, selection, window ))
                (Api.Queries.selectionDocumentsPage maskName window selection)
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

        NeedFacets selection aspects ->
            ( { cache
                | facetsValues =
                    Sort.Dict.insert ( selection, aspects ) Loading cache.facetsValues
              }
            , Api.sendQueryRequest
                (Api.withOperationName "NeedFacets")
                (ApiResponseFacets ( selection, aspects ))
                (Api.Queries.selectionFacets selection aspects config.numberOfFacetValues)
            )


{-| Insert or update a document into the table `Cache.documents`.
-}
updateWithModifiedDocument : String -> Document -> Cache -> Cache
updateWithModifiedDocument maskName document cache =
    { cache
        | documents =
            Sort.Dict.insert
                ( maskName, DocumentIdFromSearch document.id Nothing )
                (Success document)
                cache.documents
    }


{-| Digest a Msg (i.e. a response from the API layer) and update the data in the cache accordingly.
-}
update : Config -> Msg -> Cache -> ( Cache, Cmd Msg )
update config msg cache =
    case msg of
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

        ApiResponseGenericNode maskName nodeId (Ok genericNode) ->
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
                        |> targetNeeds config
                            (Needs.atomic (NeedSubfolders (List.map .id folders)))

                GenericNode.IsDocument ( document, residence ) ->
                    updateWithDocumentAndResidence
                        config
                        maskName
                        (DocumentIdFromSearch document.id Nothing)
                        (Just ( document, Just residence ))
                        cache1

                GenericNode.IsNeither ->
                    ( cache1
                    , Cmd.none
                    )

        ApiResponseGenericNode maskName nodeId (Err error) ->
            ( { cache
                | nodeTypes =
                    Sort.Dict.insert nodeId (Failure error) cache.nodeTypes
              }
            , Cmd.none
            )

        ApiResponseDocumentFromSearch maskName documentIdFromSearch (Ok maybeDocumentAndResidence) ->
            updateWithDocumentAndResidence
                config
                maskName
                documentIdFromSearch
                maybeDocumentAndResidence
                cache

        ApiResponseDocumentFromSearch maskName documentIdFromSearch (Err error) ->
            ( { cache
                | documents =
                    Sort.Dict.insert ( maskName, documentIdFromSearch ) (Failure error) cache.documents
              }
            , Cmd.none
            )

        ApiResponseDocumentsPage maskAndSelectionAndWindow result ->
            ( { cache
                | documentsPages =
                    Sort.Dict.insert maskAndSelectionAndWindow (RemoteData.fromResult result) cache.documentsPages
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

        ApiResponseFacets selectionAndAspects result ->
            ( { cache
                | facetsValues =
                    Sort.Dict.insert selectionAndAspects (RemoteData.fromResult result) cache.facetsValues
              }
            , Cmd.none
            )


updateWithDocumentAndResidence :
    Config
    -> String
    -> DocumentIdFromSearch
    -> Maybe ( Document, Maybe Residence )
    -> Cache
    -> ( Cache, Cmd Msg )
updateWithDocumentAndResidence config maskName documentIdFromSearch maybeDocumentAndResidence cache =
    let
        cache1 =
            case maybeDocumentAndResidence of
                Just ( document, _ ) ->
                    { cache
                        | documents =
                            Sort.Dict.insert
                                ( maskName, documentIdFromSearch )
                                (Success document)
                                cache.documents
                    }
                        |> insertNodeType
                            (Id.asNodeId documentIdFromSearch.id)
                            NodeIsDocument

                Nothing ->
                    cache
                        |> insertNodeType
                            (Id.asNodeId documentIdFromSearch.id)
                            NodeIsNeither

        cache2 =
            case maybeDocumentAndResidence of
                Just ( _, Just residence ) ->
                    { cache1
                        | residence =
                            Sort.Dict.insert
                                documentIdFromSearch.id
                                (Success residence)
                                cache1.residence
                    }

                Just ( _, Nothing ) ->
                    cache1

                Nothing ->
                    cache1
    in
    targetNeeds
        config
        (case maybeDocumentAndResidence of
            Just ( _, Just residence ) ->
                Needs.atomic
                    (NeedFolders
                        (Residence.toList residence)
                    )

            Just ( _, Nothing ) ->
                Needs.none

            Nothing ->
                Needs.none
        )
        cache2


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


{-| Ordering on the tuple type `( String, DocumentIdFromSearch )`
-}
orderingMaskDocumentIdFromSearch : Ordering ( String, DocumentIdFromSearch )
orderingMaskDocumentIdFromSearch =
    Ordering.byFieldWith Ordering.natural Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Types.orderingDocumentIdFromSearch Tuple.second)


{-| Ordering on the tuple type `( Selection, Window )`
-}
orderingSelectionWindow : Ordering ( Selection, Window )
orderingSelectionWindow =
    Ordering.byFieldWith Selection.orderingSelection Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Types.orderingWindow Tuple.second)


{-| Ordering on the tuple type `( String, Selection, Window )`
-}
orderingMaskSelectionWindow : Ordering ( String, Selection, Window )
orderingMaskSelectionWindow =
    Ordering.byFieldWith Ordering.natural (\( maskName, _, _ ) -> maskName)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Selection.orderingSelection (\( _, selection, _ ) -> selection))
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith Types.orderingWindow (\( _, _, window ) -> window))


{-| Ordering on the tuple type `( Selection, List String )`
-}
orderingSelectionFacets : Ordering ( Selection, List Aspect )
orderingSelectionFacets =
    Ordering.byFieldWith Selection.orderingSelectionModuloSorting Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith
                (Utils.List.lexicalOrdering Aspect.ordering)
                Tuple.second
            )
