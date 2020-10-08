module Api.Queries exposing
    ( toplevelFolder, subfolder
    , selectionDocumentsPage, selectionFolderCounts, selectionFacetByKey
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


# Folder Queries

@docs toplevelFolder, subfolder


# Document Search and Facet Queries

@docs selectionDocumentsPage, selectionFolderCounts, selectionFacetByKey


# Document Queries

@docs documentDetails


# Miscellaneous Queries

@docs genericNode, authorSearch

-}

import Api.Arguments.AttributeTest
import Api.Arguments.Filter
import Api.Fragments
import Config
import Entities.Document exposing (Document)
import Entities.DocumentResults exposing (DocumentsPage)
import Entities.Folder exposing (Folder)
import Entities.FolderCounts exposing (FolderCounts)
import Entities.GenericNode as GenericNode exposing (GenericNode)
import Entities.Residence exposing (Residence)
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import List.Nonempty exposing (Nonempty)
import Mediatum.Enum.FtsSorting
import Mediatum.InputObject
import Mediatum.Object.FoldersConnection
import Mediatum.Object.GenericNode
import Mediatum.Query
import Pagination.Relay.Connection as Connection
import Pagination.Relay.Page
import Pagination.Relay.Pagination
import Types exposing (Window)
import Types.Facet exposing (FacetValue, FacetValues)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (FacetFilters, FtsSorting(..), SelectMethod(..), Selection, SetOfFilters)


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
toplevelFolder : SelectionSet (List ( Folder, List Folder )) Graphql.Operation.RootQuery
toplevelFolder =
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
subfolder : List FolderId -> SelectionSet (List Folder) Graphql.Operation.RootQuery
subfolder folderIds =
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
            }
        }
    }

-}
genericNode : NodeId -> SelectionSet GenericNode Graphql.Operation.RootQuery
genericNode nodeId =
    let
        constructor : Maybe (Nonempty Folder) -> Maybe ( Document, Residence ) -> GenericNode
        constructor maybeLineage maybeDocumentAndResidence =
            case ( maybeLineage, maybeDocumentAndResidence ) of
                ( Just lineage, _ ) ->
                    GenericNode.IsFolder lineage

                ( Nothing, Just documentAndResidence ) ->
                    GenericNode.IsDocument documentAndResidence

                ( Nothing, Nothing ) ->
                    GenericNode.IsNeither
    in
    Mediatum.Query.genericNodeById
        { id = Id.toInt nodeId }
        (SelectionSet.succeed constructor
            |> SelectionSet.with
                (Mediatum.Object.GenericNode.asFolder
                    Api.Fragments.folderLineage
                )
            |> SelectionSet.with
                (Mediatum.Object.GenericNode.asDocument
                    (SelectionSet.succeed Tuple.pair
                        |> SelectionSet.with
                            (Api.Fragments.documentByMask "nodebig")
                        |> SelectionSet.with
                            Api.Fragments.documentResidence
                    )
                )
        )
        |> SelectionSet.nonNullOrFail


{-| Get the documents of a selection with offset-based pagination.

The selection may include a full-text-search, a list of filters and a list of facet filters.

_GraphQL notation if no FTS is involved:_

    query {
        allDocumentsPage(
            folderId: $folderId
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
    (case selection.selectMethod of
        SelectByFolderListing ->
            Mediatum.Query.allDocumentsPage
                (selectionToOptionalGraphqlArguments selection
                    >> windowToOptionalGraphqlArguments window
                )
                { folderId = selectionToFolderId selection }

        SelectByFullTextSearch searchTerm ftsSorting ->
            Mediatum.Query.ftsDocumentsPage
                (\optionals ->
                    { optionals
                        | sorting =
                            Present
                                (case ftsSorting of
                                    FtsByRank ->
                                        Mediatum.Enum.FtsSorting.ByRank

                                    FtsByDate ->
                                        Mediatum.Enum.FtsSorting.ByDate
                                )
                    }
                        |> selectionToOptionalGraphqlArguments selection
                        |> windowToOptionalGraphqlArguments window
                )
                { folderId = selectionToFolderId selection
                , text = Types.SearchTerm.toString searchTerm
                }
    )
        (Api.Fragments.documentsPage "nodesmall")
        |> SelectionSet.nonNullOrFail


{-| For a given selection get the counts of documents within a folder and its sub-folders.

The selection may include a full-text-search, a list of filters and a list of facet filters.

_GraphQL notation if no FTS is involved:_

    query {
        allDocumentsDocset(
            folderId: $folderId
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
    (case selection.selectMethod of
        SelectByFolderListing ->
            Mediatum.Query.allDocumentsDocset
                (selectionToOptionalGraphqlArguments selection)
                { folderId = selectionToFolderId selection }

        SelectByFullTextSearch searchTerm ftsSorting ->
            Mediatum.Query.ftsDocumentsDocset
                (selectionToOptionalGraphqlArguments selection)
                { folderId = selectionToFolderId selection
                , text = Types.SearchTerm.toString searchTerm
                }
    )
        Api.Fragments.folderAndSubfolderCounts
        |> SelectionSet.nonNullOrFail


{-| Get the list of values of a facet within a set of documents given by a selection.

The selection may include a full-text-search, a list of filters and a list of facet filters.

The facet in question is specified by the key of a document's attribute.

_GraphQL notation if no FTS is involved:_

    query {
        allDocumentsDocset(
            folderId: $folderId
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...facetByKey(key, limit)
        }
    }

_GraphQL notation if FTS is involved:_

    query {
        ftsDocumentsDocset(
            folderId: $folderId
            text: $searchTerm
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...facetByKey(key, limit)
        }
    }

-}
selectionFacetByKey :
    Selection
    -> String
    -> Int
    -> SelectionSet FacetValues Graphql.Operation.RootQuery
selectionFacetByKey selection key limit =
    (case selection.selectMethod of
        SelectByFolderListing ->
            Mediatum.Query.allDocumentsDocset
                (selectionToOptionalGraphqlArguments selection)
                { folderId = selectionToFolderId selection }

        SelectByFullTextSearch searchTerm ftsSorting ->
            Mediatum.Query.ftsDocumentsDocset
                (selectionToOptionalGraphqlArguments selection)
                { folderId = selectionToFolderId selection
                , text = Types.SearchTerm.toString searchTerm
                }
    )
        (Api.Fragments.facetByKey key limit)
        |> SelectionSet.nonNullOrFail


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
    Maybe (Pagination.Relay.Page.Page Document)
    -> Pagination.Relay.Pagination.Position
    -> FolderId
    -> String
    -> SelectionSet (Pagination.Relay.Page.Page Document) Graphql.Operation.RootQuery
authorSearch referencePage paginationPosition folderId searchString =
    Mediatum.Query.authorSearch
        (Pagination.Relay.Pagination.paginationArguments
            Config.pageSize
            referencePage
            paginationPosition
        )
        { folderId = Id.toInt folderId
        , text = searchString
        }
        (Connection.connection
            Api.Fragments.graphqlDocumentObjects
            (Api.Fragments.documentByMask "nodesmall")
        )
        |> SelectionSet.nonNullOrFail


{-| Get the basic properties of a document selected by its mediaTUM id
together with the document's attributes selected by the mediaTUM mask "nodebig".

_GraphQL notation:_

    query {
        documentById(id: $idOfTheDocument) {
            ...documentByMask
        }
    }

-}
documentDetails :
    DocumentId
    -> SelectionSet (Maybe ( Document, Residence )) Graphql.Operation.RootQuery
documentDetails documentId =
    Mediatum.Query.documentById
        { id = Id.toInt documentId }
        (SelectionSet.succeed Tuple.pair
            |> SelectionSet.with
                (Api.Fragments.documentByMask "nodebig")
            |> SelectionSet.with
                Api.Fragments.documentResidence
        )


selectionToFolderId : Selection -> Int
selectionToFolderId selection =
    Id.toInt selection.scope


type alias OptionalArgumentsForSelection a =
    { a
        | attributeTests : OptionalArgument (List (Maybe Mediatum.InputObject.AttributeTestInput))
    }


selectionToOptionalGraphqlArguments :
    Selection
    -> OptionalArgumentsForSelection a
    -> OptionalArgumentsForSelection a
selectionToOptionalGraphqlArguments selection optionals =
    { optionals
        | attributeTests =
            (Api.Arguments.Filter.filtersToAttributeTests selection.filters
                ++ Api.Arguments.Filter.facetFiltersToAttributeTests selection.facetFilters
            )
                |> Api.Arguments.AttributeTest.testsAsGraphqlArgument
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
