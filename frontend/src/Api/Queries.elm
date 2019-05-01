module Api.Queries exposing
    ( toplevelFolder, subfolder
    , folderDocumentsPage, folderDocumentsFolderCounts, ftsPage, ftsFolderCounts
    , documentDetails
    , genericNode
    )

{-| Definitions of all specific GraphQL queries needed in the application.


# Folder Queries

@docs toplevelFolder, subfolder


# Document Search and Facet Queries

@docs folderDocumentsPage, folderDocumentsFolderCounts, ftsPage, ftsFolderCounts


# Document Queries

@docs documentDetails


# Miscellaneous Queries

@docs genericNode

-}

import Api.Fragments
import Dict
import Document exposing (Document, DocumentId)
import DocumentResult exposing (DocumentResult)
import Folder exposing (Folder, FolderCounts, FolderId)
import GenericNode exposing (GenericNode)
import Graphql.Extra
import Graphql.Http
import Graphql.Mutation
import Graphql.Object
import Graphql.Object.Docset
import Graphql.Object.Document
import Graphql.Object.DocumentResult
import Graphql.Object.DocumentResultPage
import Graphql.Object.DocumentsConnection
import Graphql.Object.DocumentsEdge
import Graphql.Object.Folder
import Graphql.Object.FolderCount
import Graphql.Object.FolderCountsConnection
import Graphql.Object.FoldersConnection
import Graphql.Object.GenericNode
import Graphql.Object.Metadatatype
import Graphql.Object.PageInfo
import Graphql.Object.UpdateDocumentAttributePayload
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Query
import Graphql.Scalar
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode exposing (Decoder)
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Pagination.Offset.Page
import Pagination.Relay.Connection as Connection
import Pagination.Relay.Page
import Pagination.Relay.Pagination
import Query exposing (Query)
import Query.Attribute


{-| Number of results per page used for pagination.
Currently hard-coded.
-}
pageSize : Int
pageSize =
    10


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
    Graphql.Query.allFolders
        (\optionals ->
            { optionals
                | isRoot = Present True
            }
        )
        (Graphql.Object.FoldersConnection.nodes
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
    Graphql.Query.allFolders
        (\optionals ->
            { optionals
                | parentIds = List.map (Folder.idToInt >> Just) folderIds |> Present
            }
        )
        (Graphql.Object.FoldersConnection.nodes Api.Fragments.folder)


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
    Graphql.Query.genericNodeById
        (\optionals ->
            { optionals
                | id = Present nodeId
            }
        )
        (SelectionSet.succeed constructor
            |> SelectionSet.with
                (Graphql.Object.GenericNode.asFolder
                    Api.Fragments.folderLineage
                )
            |> SelectionSet.with
                (Graphql.Object.GenericNode.asDocument
                    (Api.Fragments.documentByMask "nodebig")
                )
        )
        |> SelectionSet.nonNullOrFail


{-| Get all documents of a folder with offet-based pagination
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
folderDocumentsPage :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FolderQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
folderDocumentsPage referencePage paginationPosition folderQuery =
    Graphql.Query.allDocumentsPage
        (\optionals ->
            { optionals
                | folderId = folderQuery.folder |> .id |> Folder.idToInt |> Present
                , attributeTests =
                    folderQuery.filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
                , limit = Present pageSize
                , offset =
                    Pagination.Offset.Page.positionToOffset
                        pageSize
                        referencePage
                        paginationPosition
                        |> Present
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
folderDocumentsFolderCounts :
    Query.FolderQuery
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
folderDocumentsFolderCounts folderQuery =
    Graphql.Query.allDocumentsDocset
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


{-| Get documents using a full-text search with offet-based pagination.
A list of filters may be used to restrict the documents to be found.

_GraphQL notation:_

    query {
        ftsDocumentsPage(
            folderId: $folderId
            text: $searchTerm
            domain: $attrs_or_fulltext
            language: $english_or_german
            attributeTests: $listOfAttributeTestsForFiltering
            limit: $limitNumberUsedForPagination
            offset: $offsetNumberUsedForPagination
        ) {
            ...documentResultPage
        }
    }

-}
ftsPage :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FtsQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
ftsPage referencePage paginationPosition ftsQuery =
    Graphql.Query.ftsDocumentsPage
        (\optionals ->
            { optionals
                | folderId = ftsQuery.folder |> .id |> Folder.idToInt |> Present
                , text = Present ftsQuery.searchTerm
                , domain = Present (Query.ftsOptionsDomainToString ftsQuery.options)
                , language = Present (Query.ftsOptionsLanguageToString ftsQuery.options)
                , attributeTests =
                    ftsQuery.filters
                        |> Query.filtersToAttributeTests
                        |> Query.Attribute.testsAsGraphqlArgument
                        |> Present
                , limit = Present pageSize
                , offset =
                    Pagination.Offset.Page.positionToOffset
                        pageSize
                        referencePage
                        paginationPosition
                        |> Present
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
            domain: $attrs_or_fulltext
            language: $english_or_german
            attributeTests: $listOfAttributeTestsForFiltering
        ) {
            ...folderAndSubfolderCounts
        }
    }

-}
ftsFolderCounts :
    Query.FtsQuery
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
ftsFolderCounts ftsQuery =
    Graphql.Query.ftsDocumentsDocset
        (\optionals ->
            { optionals
                | folderId = ftsQuery.folder |> .id |> Folder.idToInt |> Present
                , text = Present ftsQuery.searchTerm
                , domain = Present (Query.ftsOptionsDomainToString ftsQuery.options)
                , language = Present (Query.ftsOptionsLanguageToString ftsQuery.options)
                , attributeTests =
                    ftsQuery.filters
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
authorSearch referencePage paginationPosition folderId searchString =
    Graphql.Query.authorSearch
        ((\optionals ->
            { optionals
                | text = Present searchString
            }
         )
            >> Pagination.Relay.Pagination.paginationArguments
                pageSize
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
    Graphql.Query.documentById
        (\optionals ->
            { optionals
                | id = Present (Document.idToInt documentId)
            }
        )
        (Api.Fragments.documentByMask "nodebig")
