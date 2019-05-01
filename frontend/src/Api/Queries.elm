module Api.Queries exposing (queryToplevelFolder, querySubfolder, queryGenericNode, queryFolderDocumentsPage, queryFolderFolderCounts, queryFtsPage, queryFtsFolderCounts, queryDocumentDetails)

{-| Definitions of all specific GraphQL queries needed for the application.


# GraphQL Query Definitions

@docs queryToplevelFolder, querySubfolder, queryGenericNode, queryFolderDocumentsPage, queryFolderFolderCounts, queryFtsPage, queryFtsFolderCounts, queryDocumentDetails

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
                ...folderData
                subfolders {
                    nodes {
                        ...folderData
                    }
                }
            }
        }
    }

-}
queryToplevelFolder : SelectionSet (List ( Folder, List Folder )) Graphql.Operation.RootQuery
queryToplevelFolder =
    Graphql.Query.allFolders
        (\optionals ->
            { optionals
                | isRoot = Present True
            }
        )
        (Graphql.Object.FoldersConnection.nodes Api.Fragments.folderNodeWithSubfolders)


{-| Get the sub-folders of a list of folders.

_GraphQL notation:_

    query {
        allFolders(parentIds: $listOfFolderIds) {
            nodes {
                ...folderData
            }
        }
    }

-}
querySubfolder : List FolderId -> SelectionSet (List Folder) Graphql.Operation.RootQuery
querySubfolder folderIds =
    Graphql.Query.allFolders
        (\optionals ->
            { optionals
                | parentIds = List.map (Folder.idToInt >> Just) folderIds |> Present
            }
        )
        (Graphql.Object.FoldersConnection.nodes Api.Fragments.folderNode)


{-| Get a folder or a document with a given mediaTUM id.

_GraphQL notation:_

    query {
        genericNodeById(id: $mediatumNodeId) {
            asFolder {
                ...folderLineage
            }
            asDocument {
                ...documentNode
            }
        }
    }

-}
queryGenericNode : Int -> SelectionSet GenericNode Graphql.Operation.RootQuery
queryGenericNode nodeId =
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
                    (Api.Fragments.documentNode "nodebig")
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
queryFolderDocumentsPage :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FolderQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
queryFolderDocumentsPage referencePage paginationPosition folderQuery =
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
queryFolderFolderCounts :
    Query.FolderQuery
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
queryFolderFolderCounts folderQuery =
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
queryFtsPage :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FtsQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
queryFtsPage referencePage paginationPosition ftsQuery =
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
queryFtsFolderCounts :
    Query.FtsQuery
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
queryFtsFolderCounts ftsQuery =
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
                    ...documentNode
                }
            }
        }
    }

-}
queryAuthorSearch :
    Maybe (Pagination.Relay.Page.Page Document)
    -> Pagination.Relay.Pagination.Position
    -> FolderId
    -> String
    -> SelectionSet (Pagination.Relay.Page.Page Document) Graphql.Operation.RootQuery
queryAuthorSearch referencePage paginationPosition folderId searchString =
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
            (Api.Fragments.documentNode "nodesmall")
        )


{-| Get the basic properties of a document selected by its mediaTUM id
together with the document's attributes selected by the mediaTUM mask "nodebig".

_GraphQL notation:_

    query {
        documentById(id: $idOfTheDocument) {
            ...documentNode
        }
    }

-}
queryDocumentDetails :
    DocumentId
    -> SelectionSet (Maybe Document) Graphql.Operation.RootQuery
queryDocumentDetails documentId =
    Graphql.Query.documentById
        (\optionals ->
            { optionals
                | id = Present (Document.idToInt documentId)
            }
        )
        (Api.Fragments.documentNode "nodebig")
