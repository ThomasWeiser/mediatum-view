module Api.Queries exposing
    ( toplevelFolder, subfolder
    , folderDocumentsPage, folderDocumentsFolderCounts, ftsPage, ftsFolderCounts
    , documentDetails
    , genericNode, authorSearch
    , folderDocumentsFolderCounts_ByQuery, folderDocumentsPage_ByQuery, ftsFolderCounts_ByQuery, ftsPage_ByQuery
    )

{-| Definitions of all specific GraphQL queries needed in the application.

Please note:
For documenting the individual query functions we show the equivalent GraphQL notation.

Many of these functions will refer to other SelectionSet-returning functions
for nested subqueries.
We will use the GraphQL fragment notation for denoting this embedding.
In reality it's just function calling.
The `elm-graphql` package won't use the fragment notation.


# Folder Queries

@docs toplevelFolder, subfolder


# Document Search and Facet Queries

@docs folderDocumentsPage, folderDocumentsFolderCounts, ftsPage, ftsFolderCounts


# Document Queries

@docs documentDetails


# Miscellaneous Queries

@docs genericNode, authorSearch

-}

import Api.Fragments
import Config
import Data.Types exposing (..)
import Document
import Folder
import GenericNode exposing (GenericNode)
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import List.Nonempty exposing (Nonempty)
import Mediatum.Enum.FtsSorting
import Mediatum.Object.FoldersConnection
import Mediatum.Object.GenericNode
import Mediatum.Query
import Pagination.Offset.Page
import Pagination.Relay.Connection as Connection
import Pagination.Relay.Page
import Pagination.Relay.Pagination
import Query
import Query.Attribute


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
                | parentIds = List.map (Folder.idToInt >> Just) folderIds |> Present
            }
        )
        (Mediatum.Object.FoldersConnection.nodes Api.Fragments.folder)


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
genericNode : Int -> SelectionSet GenericNode Graphql.Operation.RootQuery
genericNode nodeId =
    let
        constructor : Maybe (Nonempty Folder) -> Maybe Document -> GenericNode
        constructor maybeLineage maybeDocument =
            case ( maybeLineage, maybeDocument ) of
                ( Just lineage, _ ) ->
                    GenericNode.IsFolder lineage

                ( Nothing, Just document ) ->
                    GenericNode.IsDocument document

                ( Nothing, Nothing ) ->
                    GenericNode.IsNeither
    in
    Mediatum.Query.genericNodeById
        (\optionals ->
            { optionals
                | id = Present nodeId
            }
        )
        (SelectionSet.succeed constructor
            |> SelectionSet.with
                (Mediatum.Object.GenericNode.asFolder
                    Api.Fragments.folderLineage
                )
            |> SelectionSet.with
                (Mediatum.Object.GenericNode.asDocument
                    (Api.Fragments.documentByMask "nodebig")
                )
        )
        |> SelectionSet.nonNullOrFail


{-| Get all documents of a folder with offet-based pagination.

A list of filters may be used to restrict the documents to be returned.

_GraphQL notation:_

    query {
        allDocumentsPage(
            folderId: $folderId
            attributeTests: $listOfAttributeTestsForFiltering
            limit: $limitNumberUsedForPagination
            offset: $offsetNumberUsedForPagination
        ) {
            ...documentResultPage
        }
    }

-}
folderDocumentsPage_ByQuery :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FolderQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
folderDocumentsPage_ByQuery referencePage paginationPosition folderQuery =
    Mediatum.Query.allDocumentsPage
        (\optionals ->
            { optionals
                | folderId = folderQuery.folder |> .id |> Folder.idToInt |> Present
                , attributeTests =
                    folderQuery.filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
                , limit = Present Config.pageSize
                , offset =
                    Pagination.Offset.Page.positionToOffset
                        Config.pageSize
                        referencePage
                        paginationPosition
                        |> Present
            }
        )
        (Api.Fragments.documentResultPage "nodesmall")
        |> SelectionSet.nonNullOrFail


folderDocumentsPage :
    Window
    -> FolderId
    -> Filters
    -> SelectionSet DocumentsPage Graphql.Operation.RootQuery
folderDocumentsPage window folderId filters =
    Mediatum.Query.allDocumentsPage
        (\optionals ->
            { optionals
                | folderId = Present (Folder.idToInt folderId)
                , attributeTests =
                    filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
                , limit = Present window.limit
                , offset = Present window.offset
            }
        )
        (Api.Fragments.documentResultPage "nodesmall")
        |> SelectionSet.nonNullOrFail


{-| Get the counts of documents within a folder and its sub-folders.

A list of filters may be used to restrict the documents to be counted.

_GraphQL notation:_

    query {
        allDocumentsDocset(
            folderId: $folderId
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...folderAndSubfolderCounts
        }
    }

-}
folderDocumentsFolderCounts_ByQuery :
    Query.FolderQuery
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
folderDocumentsFolderCounts_ByQuery folderQuery =
    Mediatum.Query.allDocumentsDocset
        (\optionals ->
            { optionals
                | folderId = folderQuery.folder |> .id |> Folder.idToInt |> Present
                , attributeTests =
                    folderQuery.filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
            }
        )
        Api.Fragments.folderAndSubfolderCounts
        |> SelectionSet.nonNullOrFail


folderDocumentsFolderCounts :
    FolderId
    -> Filters
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
folderDocumentsFolderCounts folderId filters =
    Mediatum.Query.allDocumentsDocset
        (\optionals ->
            { optionals
                | folderId = Present (Folder.idToInt folderId)
                , attributeTests =
                    filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
            }
        )
        Api.Fragments.folderAndSubfolderCounts
        |> SelectionSet.nonNullOrFail


{-| Get documents using a full-text search with offet-based pagination.

A list of filters may be used to restrict the documents to be found.

_GraphQL notation:_

    query {
        ftsDocumentsPage(
            folderId: $folderId
            text: $searchTerm
            orderBy: $RANKING_or_DATE
            attributeTests: $listOfAttributeTestsForFiltering
            limit: $limitNumberUsedForPagination
            offset: $offsetNumberUsedForPagination
        ) {
            ...documentResultPage
        }
    }

-}
ftsPage_ByQuery :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FtsQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
ftsPage_ByQuery referencePage paginationPosition ftsQuery =
    Mediatum.Query.ftsDocumentsPage
        (\optionals ->
            { optionals
                | folderId = ftsQuery.folder |> .id |> Folder.idToInt |> Present
                , text = Present ftsQuery.searchTerm
                , sorting =
                    Present
                        (case ftsQuery.sorting of
                            Query.ByRank ->
                                Mediatum.Enum.FtsSorting.ByRank

                            Query.ByDate ->
                                Mediatum.Enum.FtsSorting.ByDate
                        )
                , attributeTests =
                    ftsQuery.filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
                , limit = Present Config.pageSize
                , offset =
                    Pagination.Offset.Page.positionToOffset
                        Config.pageSize
                        referencePage
                        paginationPosition
                        |> Present
            }
        )
        (Api.Fragments.documentResultPage "nodesmall")
        |> SelectionSet.nonNullOrFail


ftsPage :
    Window
    -> FolderId
    -> String
    -> FtsSorting
    -> Filters
    -> SelectionSet DocumentsPage Graphql.Operation.RootQuery
ftsPage window folderId searchTerm ftsSorting filters =
    Mediatum.Query.ftsDocumentsPage
        (\optionals ->
            { optionals
                | folderId = Present (Folder.idToInt folderId)
                , text = Present searchTerm
                , sorting =
                    Present
                        (case ftsSorting of
                            FtsByRank ->
                                Mediatum.Enum.FtsSorting.ByRank

                            FtsByDate ->
                                Mediatum.Enum.FtsSorting.ByDate
                        )
                , attributeTests =
                    filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
                , limit = Present window.limit
                , offset = Present window.offset
            }
        )
        (Api.Fragments.documentResultPage "nodesmall")
        |> SelectionSet.nonNullOrFail


{-| Get the counts of documents found by a full-text search.

The counts are computed for the given folder and each of its sub-folders.

A list of filters may be used to restrict the documents to be counted.

_GraphQL notation:_

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
ftsFolderCounts_ByQuery :
    Query.FtsQuery
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
ftsFolderCounts_ByQuery ftsQuery =
    Mediatum.Query.ftsDocumentsDocset
        (\optionals ->
            { optionals
                | folderId = ftsQuery.folder |> .id |> Folder.idToInt |> Present
                , text = Present ftsQuery.searchTerm
                , attributeTests =
                    ftsQuery.filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
            }
        )
        Api.Fragments.folderAndSubfolderCounts
        |> SelectionSet.nonNullOrFail


ftsFolderCounts :
    FolderId
    -> String
    -> FtsSorting
    -> Filters
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
ftsFolderCounts folderId searchTerm ftsSorting filters =
    Mediatum.Query.ftsDocumentsDocset
        (\optionals ->
            { optionals
                | folderId = Present (Folder.idToInt folderId)
                , text = Present searchTerm
                , attributeTests =
                    filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
            }
        )
        Api.Fragments.folderAndSubfolderCounts
        |> SelectionSet.nonNullOrFail


{-| Get a page of documents found by searching on an author's name.

The result is paginated according to the Relay specification.

Up to now this query function is only a preliminary draft.

_GraphQL notation:_

    query {
        authorSearch(
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
authorSearch referencePage paginationPosition _ searchString =
    Mediatum.Query.authorSearch
        ((\optionals ->
            { optionals
                | text = Present searchString
            }
         )
            >> Pagination.Relay.Pagination.paginationArguments
                Config.pageSize
                referencePage
                paginationPosition
        )
        (Connection.connection
            Api.Fragments.graphqlDocumentObjects
            (Api.Fragments.documentByMask "nodesmall")
        )


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
    -> SelectionSet (Maybe Document) Graphql.Operation.RootQuery
documentDetails documentId =
    Mediatum.Query.documentById
        (\optionals ->
            { optionals
                | id = Present (Document.idToInt documentId)
            }
        )
        (Api.Fragments.documentByMask "nodebig")
