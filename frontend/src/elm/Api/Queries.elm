module Api.Queries exposing
    ( serverSetup
    , toplevelFolders, folders, subfolders
    , selectionDocumentsPage, selectionFolderCounts, selectionFacets
    , documentDetails
    , genericNode, authorSearch
    )

{-| Definitions of all specific GraphQL queries needed in the application.

For documenting the individual query functions we show the equivalent GraphQL notation.

Please note:
Many of these functions will refer to other SelectionSet-returning functions
for nested subqueries.
We will use the GraphQL fragment notation for denoting this embedding.
In reality it's just function calling.
(The `elm-graphql` package won't use the fragment notation.)


# Server Config

@docs serverSetup


# Folder Queries

@docs toplevelFolders, folders, subfolders


# Document Search and Facet Queries

@docs selectionDocumentsPage, selectionFolderCounts, selectionFacets


# Document Queries

@docs documentDetails


# Miscellaneous Queries

@docs genericNode, authorSearch

-}

import Api.Arguments.AspectTest
import Api.Arguments.Filter
import Api.Fragments
import Constants
import Entities.Document exposing (Document)
import Entities.DocumentResults exposing (DocumentsPage)
import Entities.Folder exposing (Folder, LineageFolders)
import Entities.FolderCounts exposing (FolderCounts)
import Entities.GenericNode as GenericNode exposing (GenericNode)
import Entities.Residence exposing (Residence)
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Mediatum.Enum.FtsSorting
import Mediatum.InputObject
import Mediatum.Object
import Mediatum.Object.FacetAspectConfig
import Mediatum.Object.FoldersConnection
import Mediatum.Object.FtsAspectConfig
import Mediatum.Object.GenericNode
import Mediatum.Object.Setup
import Mediatum.Object.SetupConfig
import Mediatum.Query
import Pagination.Relay.Connection as Connection
import Pagination.Relay.Page
import Pagination.Relay.Pagination
import Types exposing (DocumentIdFromSearch, Window)
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config.FacetAspectConfig exposing (FacetAspectConfig)
import Types.Config.FtsAspectConfig exposing (FtsAspectConfig)
import Types.FacetValue as Facet exposing (FacetsValues)
import Types.FilterList as FilterList
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm
import Types.Selection exposing (Selection, Sorting(..))
import Types.ServerSetup as ServerSetup exposing (ServerSetup)


{-| -}
serverSetup : SelectionSet ServerSetup Graphql.Operation.RootQuery
serverSetup =
    Mediatum.Query.setup
        { application = "hsb"
        , clientName = "mediatum-view-web"
        , clientVersion = "1.0"
        }
        (SelectionSet.succeed ServerSetup.ServerSetup
            |> SelectionSet.with
                (Mediatum.Object.Setup.config
                    (SelectionSet.succeed ServerSetup.ServerConfig
                        |> SelectionSet.with
                            Mediatum.Object.SetupConfig.defaultPageSize
                        |> SelectionSet.with
                            (Mediatum.Object.SetupConfig.defaultSorting
                                |> SelectionSet.map
                                    (Maybe.map
                                        (\apiEnum ->
                                            case apiEnum of
                                                Mediatum.Enum.FtsSorting.ByRank ->
                                                    ByRank

                                                Mediatum.Enum.FtsSorting.ByDate ->
                                                    ByDate
                                        )
                                    )
                            )
                        |> SelectionSet.with
                            Mediatum.Object.SetupConfig.numberOfFacetValues
                        |> SelectionSet.with
                            (Mediatum.Object.SetupConfig.staticFtsAspects
                                Api.Fragments.ftsAspectConfig
                                |> Api.Fragments.nonNullElementsOfMaybeListOrFail
                            )
                        |> SelectionSet.with
                            (Mediatum.Object.SetupConfig.staticFacetAspects
                                Api.Fragments.facetAspectConfig
                                |> Api.Fragments.nonNullElementsOfMaybeListOrFail
                            )
                    )
                    |> SelectionSet.nonNullOrFail
                )
        )
        |> SelectionSet.nonNullOrFail


{-| Get the root folders and their sub-folders.

_GraphQL notation:_

    query {
        allFolders(isRoot: true) {
            nodes {
                ...folderAndSubfolders
            }
        }
    }

-}
toplevelFolders : SelectionSet (List ( Folder, List Folder )) Graphql.Operation.RootQuery
toplevelFolders =
    Mediatum.Query.allFolders
        (\optionals ->
            { optionals
                | isRoot = Present True
            }
        )
        (Mediatum.Object.FoldersConnection.nodes
            Api.Fragments.folderAndSubfolders
        )
        |> SelectionSet.nonNullOrFail


{-| Get the folders by a list of folder ids.

_GraphQL notation:_

    query {
        allFolders(ids: $listOfFolderIds) {
            nodes {
                ...folder
            }
        }
    }

-}
folders : List FolderId -> SelectionSet (List Folder) Graphql.Operation.RootQuery
folders folderIds =
    Mediatum.Query.allFolders
        (\optionals ->
            { optionals
                | ids = List.map (Id.toInt >> Just) folderIds |> Present
            }
        )
        (Mediatum.Object.FoldersConnection.nodes Api.Fragments.folder)
        |> SelectionSet.nonNullOrFail


{-| Get the sub-folders of a list of folders.

_GraphQL notation:_

    query {
        allFolders(parentIds: $listOfFolderIds) {
            nodes {
                ...folder
            }
        }
    }

-}
subfolders : List FolderId -> SelectionSet (List Folder) Graphql.Operation.RootQuery
subfolders folderIds =
    Mediatum.Query.allFolders
        (\optionals ->
            { optionals
                | parentIds = List.map (Id.toInt >> Just) folderIds |> Present
            }
        )
        (Mediatum.Object.FoldersConnection.nodes Api.Fragments.folder)
        |> SelectionSet.nonNullOrFail


{-| Get a folder or a document with a given mediaTUM id.

_GraphQL notation:_

    query {
        genericNodeById(id: $mediatumNodeId) {
            asFolder {
                ...folderLineage
            }
            asDocument {
                ...documentByMask
                ...documentResidence
            }
        }
    }

-}
genericNode : NodeId -> SelectionSet GenericNode Graphql.Operation.RootQuery
genericNode nodeId =
    let
        constructor : Maybe LineageFolders -> Maybe ( Document, Residence ) -> GenericNode
        constructor maybeLineage maybeDocumentAndResidence =
            case ( maybeLineage, maybeDocumentAndResidence ) of
                ( Just lineage, _ ) ->
                    GenericNode.IsFolder lineage

                ( Nothing, Just documentAndResidence ) ->
                    GenericNode.IsDocument documentAndResidence

                ( Nothing, Nothing ) ->
                    -- Node exists, but is neither a folder nor a document
                    GenericNode.IsNeither
    in
    Mediatum.Query.genericNodeById
        { id = Id.toInt nodeId }
        (SelectionSet.succeed constructor
            |> SelectionSet.with
                (Mediatum.Object.GenericNode.asFolder
                    Api.Fragments.folderLineageFolders
                )
            |> SelectionSet.with
                (Mediatum.Object.GenericNode.asDocument
                    (SelectionSet.succeed Tuple.pair
                        |> SelectionSet.with
                            (Api.Fragments.documentByMask "nodebig" Nothing)
                        |> SelectionSet.with
                            Api.Fragments.documentResidence
                    )
                )
        )
        |> SelectionSet.map
            (Maybe.withDefault
                -- Node doesn't exist
                GenericNode.IsNeither
            )


{-| Get the documents of a selection with offset-based pagination.

The selection may include a full-text-search, a list of filters and a list of facet filters.

_GraphQL notation if no FTS is involved:_

    query {
        allDocumentsPage(
            folderId: $folderId
            aspectTests: $listOfAspectTestsForFiltering
            attributeTests: $listOfAttributeTestsForFiltering
            limit: $limitNumberUsedForPagination
            offset: $offsetNumberUsedForPagination
        ) {
            ...documentsPage
        }
    }

_GraphQL notation if FTS is involved:_

    query {
        ftsDocumentsPage(
            folderId: $folderId
            text: $searchTerm
            orderBy: $RANKING_or_DATE
            aspectTests: $listOfAspectTestsForFiltering
            attributeTests: $listOfAttributeTestsForFiltering
            limit: $limitNumberUsedForPagination
            offset: $offsetNumberUsedForPagination
        ) {
            ...documentsPage
        }
    }

-}
selectionDocumentsPage :
    Window
    -> Selection
    -> SelectionSet DocumentsPage Graphql.Operation.RootQuery
selectionDocumentsPage window selection =
    let
        ( query, maybeSearchTerm ) =
            if selection.globalFts == Nothing && FilterList.isEmpty selection.ftsFilters then
                ( Mediatum.Query.allDocumentsPage
                    (selectionToOptionalGraphqlArguments selection
                        >> windowToOptionalGraphqlArguments window
                    )
                    { folderId = selectionToFolderId selection }
                , Nothing
                )

            else
                ( Mediatum.Query.ftsDocumentsPage
                    (\optionals ->
                        { optionals
                            | sorting =
                                Present
                                    (case selection.sorting of
                                        ByRank ->
                                            Mediatum.Enum.FtsSorting.ByRank

                                        ByDate ->
                                            Mediatum.Enum.FtsSorting.ByDate
                                    )
                        }
                            |> selectionToOptionalGraphqlArguments selection
                            |> windowToOptionalGraphqlArguments window
                    )
                    { folderId = selectionToFolderId selection
                    , text =
                        -- The text parameter is mandatory in the current API.
                        -- Nonetheless we can set is to the empty string as long as we have some ftsFilters.
                        Maybe.Extra.unwrap "" Types.SearchTerm.toString selection.globalFts
                    }
                , selection.globalFts
                )
    in
    query
        (Api.Fragments.documentsPage "nodesmall" maybeSearchTerm)
        |> SelectionSet.nonNullOrFail


{-| For a given selection get the counts of documents within a folder and its sub-folders.

The selection may include a full-text-search, a list of filters and a list of facet filters.

_GraphQL notation if no FTS is involved:_

    query {
        allDocumentsDocset(
            folderId: $folderId
            aspectTests: $listOfAspectTestsForFiltering
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...folderAndSubfolderCounts
        }
    }

_GraphQL notation if FTS is involved:_

    query {
        ftsDocumentsDocset(
            folderId: $folderId
            text: $searchTerm
            aspectTests: $listOfAspectTestsForFiltering
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...folderAndSubfolderCounts
        }
    }

-}
selectionFolderCounts :
    Selection
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
selectionFolderCounts selection =
    (if selection.globalFts == Nothing && FilterList.isEmpty selection.ftsFilters then
        Mediatum.Query.allDocumentsDocset
            (selectionToOptionalGraphqlArguments selection)
            { folderId = selectionToFolderId selection }

     else
        Mediatum.Query.ftsDocumentsDocset
            (selectionToOptionalGraphqlArguments selection)
            { folderId = selectionToFolderId selection
            , text =
                -- The text parameter is mandatory in the current API.
                -- Nonetheless we can set is to the empty string as long as we have some ftsFilters.
                Maybe.Extra.unwrap "" Types.SearchTerm.toString selection.globalFts
            }
    )
        Api.Fragments.folderAndSubfolderCounts
        |> SelectionSet.nonNullOrFail


{-| Get the list of values of a list of facets within a set of documents given by a selection.

The selection may include a full-text-search, a list of filters and a list of facet filters.

The facets in question are specified by are list of names of the corresponding aspects.

_GraphQL notation if no FTS is involved:_

    query {
        allDocumentsDocset(
            folderId: $folderId
            aspectTests: $listOfAspectTestsForFiltering
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...facetByAspect(aspect1, limit)
            ...facetByAspect(aspect2, limit)
        }
    }

_GraphQL notation if FTS is involved:_

    query {
        ftsDocumentsDocset(
            folderId: $folderId
            text: $searchTerm
            aspectTests: $listOfAspectTestsForFiltering
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...facetByAspect(aspect1, limit)
            ...facetByAspect(aspect2, limit)
        }
    }

-}
selectionFacets :
    Selection
    -> List Aspect
    -> Int
    -> SelectionSet FacetsValues Graphql.Operation.RootQuery
selectionFacets selection aspects limit =
    (if selection.globalFts == Nothing && FilterList.isEmpty selection.ftsFilters then
        Mediatum.Query.allDocumentsDocset
            (selectionToOptionalGraphqlArguments selection)
            { folderId = selectionToFolderId selection }

     else
        Mediatum.Query.ftsDocumentsDocset
            (selectionToOptionalGraphqlArguments selection)
            { folderId = selectionToFolderId selection
            , text =
                -- The text parameter is mandatory in the current API.
                -- Nonetheless we can set is to the empty string as long as we have some ftsFilters.
                Maybe.Extra.unwrap "" Types.SearchTerm.toString selection.globalFts
            }
    )
        (SelectionSet.dict
            (List.map
                (\aspect ->
                    ( Aspect.toString aspect
                    , Api.Fragments.facetByAspect aspect limit
                    )
                )
                aspects
            )
        )
        |> SelectionSet.nonNullOrFail
        |> SelectionSet.map Facet.facetsValuesFromDict


{-| Get a page of documents found by searching on an author's name.

The result is paginated according to the Relay specification.

Up to now this query function is only a preliminary draft.

_GraphQL notation:_

    query {
        authorSearch(
            folderId: $folderId
            text: $searchTerm
            first: $optionalRelayPaginationArgument
            last: $optionalRelayPaginationArgument
            before: $optionalRelayPaginationArgument
            after: $optionalRelayPaginationArgument
        ) {
            edges {
                node {
                    ...documentByMask
                }
            }
        }
    }

-}
authorSearch :
    Int
    -> Maybe (Pagination.Relay.Page.Page Document)
    -> Pagination.Relay.Pagination.Position
    -> FolderId
    -> String
    -> SelectionSet (Pagination.Relay.Page.Page Document) Graphql.Operation.RootQuery
authorSearch pageSize referencePage paginationPosition folderId searchString =
    Mediatum.Query.authorSearch
        (Pagination.Relay.Pagination.paginationArguments
            pageSize
            referencePage
            paginationPosition
        )
        { folderId = Id.toInt folderId
        , text = searchString
        }
        (Connection.connection
            Api.Fragments.graphqlDocumentObjects
            (Api.Fragments.documentByMask "nodesmall" Nothing)
        )
        |> SelectionSet.nonNullOrFail


{-| Get the basic properties of a document selected by its mediaTUM id
together with the document's attributes selected by the mediaTUM mask "nodebig"
and the document's residence.

_GraphQL notation:_

    query {
        documentById(id: $idOfTheDocument) {
            ...documentByMask
            ...documentResidence
        }
    }

-}
documentDetails :
    DocumentIdFromSearch
    -> Bool
    -> SelectionSet (Maybe ( Document, Maybe Residence )) Graphql.Operation.RootQuery
documentDetails documentIdFromSearch withResidence =
    Mediatum.Query.documentById
        { id = Id.toInt documentIdFromSearch.id }
        (SelectionSet.succeed Tuple.pair
            |> SelectionSet.with
                (Api.Fragments.documentByMask "nodebig" documentIdFromSearch.search)
            |> (if withResidence then
                    SelectionSet.with
                        (Api.Fragments.documentResidence
                            |> SelectionSet.map Just
                        )

                else
                    SelectionSet.hardcoded Nothing
               )
        )


selectionToFolderId : Selection -> Int
selectionToFolderId selection =
    Id.toInt selection.scope


type alias OptionalArgumentsForSelection a =
    { a
        | attributeTests : OptionalArgument (List (Maybe Mediatum.InputObject.AttributeTestInput))
        , aspectTests : OptionalArgument (List (Maybe Mediatum.InputObject.AspectTestInput))
    }


selectionToOptionalGraphqlArguments :
    Selection
    -> OptionalArgumentsForSelection a
    -> OptionalArgumentsForSelection a
selectionToOptionalGraphqlArguments selection optionals =
    if FilterList.isEmpty selection.ftsFilters && FilterList.isEmpty selection.facetFilters then
        optionals

    else
        { optionals
            | aspectTests =
                List.append
                    (Api.Arguments.Filter.ftsFiltersToAspectTests selection.ftsFilters)
                    (Api.Arguments.Filter.facetFiltersToAspectTests selection.facetFilters)
                    |> Api.Arguments.AspectTest.testsAsGraphqlArgument
                    |> Present
        }


type alias OptionalArgumentsForWindow a =
    { a
        | limit : OptionalArgument Int
        , offset : OptionalArgument Int
    }


windowToOptionalGraphqlArguments :
    Window
    -> OptionalArgumentsForWindow a
    -> OptionalArgumentsForWindow a
windowToOptionalGraphqlArguments window optionals =
    { optionals
        | limit = Present window.limit
        , offset = Present window.offset
    }
