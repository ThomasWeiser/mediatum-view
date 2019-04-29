module Api exposing
    ( Response, ApiError
    , makeQueryRequest, makeMutationRequest
    , queryToplevelFolder, querySubfolder, queryGenericNode, queryFolderDocumentsPage, queryFolderFolderCounts, queryFtsPage, queryFtsFolderCounts, queryDocumentDetails
    , updateDocumentAttribute
    )

{-| Definitions of all specific GraphQL requests needed for the application.


# Types

@docs Response, ApiError


# Run GraphQL Requests

@docs makeQueryRequest, makeMutationRequest


# GraphQL Query Definitions

@docs queryToplevelFolder, querySubfolder, queryGenericNode, queryFolderDocumentsPage, queryFolderFolderCounts, queryFtsPage, queryFtsFolderCounts, queryDocumentDetails


# GraphQL Mutation Definitions

@docs updateDocumentAttribute

-}

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


{-| Endpoint for backend's GraphQL service.
-}
apiUrl : String
apiUrl =
    "/graphql"


{-| Number of results per page used for pagination.
Currently hard-coded.
-}
pageSize : Int
pageSize =
    10


{-| A query specific Result type.
-}
type alias Response decodesTo =
    Result ApiError decodesTo


{-| Represents an error from running a GraphQL request.
-}
type alias ApiError =
    Graphql.Extra.StrippedError


{-| Create a GraphQL query.

Takes a tagger function for wrapping the result,
which is either a query specific type representing the queried data
or an ApiError.

The query itself is given as a `Graphql.SelectionSet.SelectionSet`
, see [elm-graphql](https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest/Graphql-SelectionSet).
There are functions in this module to produce these selection sets for all
relevant queries of the application.

-}
makeQueryRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootQuery
    -> Cmd msg
makeQueryRequest tagger selectionSet =
    selectionSet
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send
            (Result.mapError Graphql.Extra.stripError >> tagger)


{-| Create a GraphQL mutation.

Like `makeQueryRequest` but for mutations.

-}
makeMutationRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootMutation
    -> Cmd msg
makeMutationRequest tagger selectionSet =
    selectionSet
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send
            (Result.mapError Graphql.Extra.stripError >> tagger)


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
        (Graphql.Object.FoldersConnection.nodes folderNodeWithSubfolders)


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
        (Graphql.Object.FoldersConnection.nodes folderNode)


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
            |> SelectionSet.with (Graphql.Object.GenericNode.asFolder folderLineage)
            |> SelectionSet.with (Graphql.Object.GenericNode.asDocument (documentNode "nodebig"))
        )
        |> SelectionSet.nonNullOrFail


{-| Selection set on a Folder to get basic properties of the folder.

_GraphQL notation:_

    fragment folderData on Folder {
        id
        parentId
        name
        isCollection
        numSubfolder
    }

-}
folderNode : SelectionSet Folder Graphql.Object.Folder
folderNode =
    SelectionSet.succeed Folder.init
        |> SelectionSet.with (Graphql.Object.Folder.id |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with Graphql.Object.Folder.parentId
        |> SelectionSet.with (Graphql.Object.Folder.name |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with (Graphql.Object.Folder.isCollection |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with (Graphql.Object.Folder.numSubfolder |> SelectionSet.nonNullOrFail)


{-| Selection set on a folder to get the basic properties of that folder and of its sub-folders.

_GraphQL notation:_

    fragment folderNodeWithSubfolders on Folder {
        ...folderNode
        subfolders {
            nodes {
                ...folderNode
            }
        }
    }

-}
folderNodeWithSubfolders : SelectionSet ( Folder, List Folder ) Graphql.Object.Folder
folderNodeWithSubfolders =
    SelectionSet.succeed Tuple.pair
        |> SelectionSet.with folderNode
        |> SelectionSet.with
            (Graphql.Object.Folder.subfolders identity
                (SelectionSet.succeed identity
                    |> SelectionSet.with (Graphql.Object.FoldersConnection.nodes folderNode)
                )
            )


{-| Selection set on a folder to get the lineage of that folder,
i.e. a non-emtpy list of folders representing the path from the given folder up to a root folder of the hierarchy.

_GraphQL notation:_

    fragment folderLineage on Folder {
        lineage {
            ...folderData
        }
    }

-}
folderLineage : SelectionSet (Nonempty Folder) Graphql.Object.Folder
folderLineage =
    Graphql.Object.Folder.lineage
        folderNode
        |> SelectionSet.nonNullOrFail
        |> SelectionSet.nonNullElementsOrFail
        |> SelectionSet.mapOrFail
            (List.Nonempty.fromList
                >> Result.fromMaybe "Lineage needs at least one folder"
            )


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
        (documentResultPage "nodesmall")
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
        folderAndSubfolderCounts
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
        (documentResultPage "nodesmall")
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
        folderAndSubfolderCounts
        |> SelectionSet.nonNullOrFail


{-| Selection set on a Docset to get the counts of documents within.

_GraphQL notation:_

    fragment folderAndSubfolderCounts on Docset {
        folderCount {
            ...folderCount
        }
        subfolderCounts {
            nodes {
                ...folderCount
            }
        }
    }

-}
folderAndSubfolderCounts : SelectionSet Folder.FolderCounts Graphql.Object.Docset
folderAndSubfolderCounts =
    SelectionSet.succeed
        (\pair listOfPairs -> Dict.fromList (pair :: listOfPairs))
        |> SelectionSet.with
            (Graphql.Object.Docset.folderCount
                folderCount
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Docset.subfolderCounts
                identity
                (SelectionSet.succeed identity
                    |> SelectionSet.with
                        (Graphql.Object.FolderCountsConnection.nodes
                            folderCount
                        )
                )
            )


{-| Selection set on a FolderCount to get the count of the selected documents within the folder.

_GraphQL notation:_

    fragment folderCount on FolderCount {
        count
        folderId
    }

-}
folderCount : SelectionSet ( FolderId, Int ) Graphql.Object.FolderCount
folderCount =
    SelectionSet.succeed Tuple.pair
        |> SelectionSet.with
            (Graphql.Object.FolderCount.folderId
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map Folder.idFromInt
            )
        |> SelectionSet.with
            (Graphql.Object.FolderCount.count
                |> SelectionSet.nonNullOrFail
            )


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
            graphqlDocumentObjects
            (documentNode "nodesmall")
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
        (documentNode "nodebig")


{-| Set an attribute of a document selected by a mediaTUM id.
Returns the new document details based on the mediaTUM mask "nodebig".

_GraphQL notation:_

    mutation {
        updateDocumentAttribute(
            input: {
                id: $idOfTheDocument
                key: $keyOfTheAttribute
                value: $desiredValueOfTheAttribute
            }
        ) {
            ...documentNode
        }
    }

-}
updateDocumentAttribute :
    DocumentId
    -> String
    -> String
    -> SelectionSet (Maybe Document) Graphql.Operation.RootMutation
updateDocumentAttribute documentId key value =
    SelectionSet.map Maybe.Extra.join
        (Graphql.Mutation.updateDocumentAttribute
            { input =
                { clientMutationId = Absent
                , id = Present (Document.idToInt documentId)
                , key = Present key
                , value = Present value
                }
            }
            (SelectionSet.succeed identity
                |> SelectionSet.with
                    (Graphql.Object.UpdateDocumentAttributePayload.document
                        (documentNode "nodebig")
                    )
            )
        )


{-| Selection set on a DocumentResultPage to get a page of a paginated list of documents.

The page contains a list of document results as well as some pagination-specific data.

The nested documents are rendered according to a named mediaTUM mask.

_GraphQL notation:_

    fragment documentResultPage on DocumentResultPage {
        offset
        hasNextPage
        content {
            ...documentResult
        }
    }

-}
documentResultPage :
    String
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Object.DocumentResultPage
documentResultPage maskName =
    SelectionSet.succeed Pagination.Offset.Page.Page
        |> SelectionSet.with
            (Graphql.Object.DocumentResultPage.offset
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResultPage.hasNextPage
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResultPage.content
                (documentResult maskName)
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.nonNullElementsOrFail
            )


{-| Selection set on a DocumentResult to get the document data and additional search result characterization
(position of the result, ranking of the result).

The nested document is rendered according to a named mediaTUM mask.

_GraphQL notation:_

    fragment documentResult on DocumentResult {
        number
        distance
        ...documentNode
    }

-}
documentResult : String -> SelectionSet DocumentResult Graphql.Object.DocumentResult
documentResult maskName =
    SelectionSet.succeed DocumentResult.init
        |> SelectionSet.with
            (Graphql.Object.DocumentResult.number
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResult.distance
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResult.document
                (documentNode maskName)
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a Document to get the basic properties of the document
together with the document's attributes selected by a named mediaTUM mask.

_GraphQL notation:_

    fragment documentNode on Document {
        id
        metadatatype {
            longname
        }
        name
        valuesByMask(maskName: maskName)
    }

-}
documentNode : String -> SelectionSet Document Graphql.Object.Document
documentNode maskName =
    SelectionSet.succeed Document.init
        |> SelectionSet.with
            (Graphql.Object.Document.id
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Document.metadatatype
                (SelectionSet.succeed identity
                    |> SelectionSet.with
                        (Graphql.Object.Metadatatype.longname
                            |> SelectionSet.nonNullOrFail
                        )
                )
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Document.name
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Document.valuesByMask
                (\optionals ->
                    { optionals
                        | maskName = Present maskName
                    }
                )
                |> SelectionSet.map mapJsonToAttributes
            )


{-| Decode a JSON string returned from a query that denotes the mata-values of a document.
-}
mapJsonToAttributes : Maybe Graphql.Scalar.Json -> List Document.Attribute
mapJsonToAttributes maybeJson =
    case maybeJson of
        Nothing ->
            []

        Just (Graphql.Scalar.Json str) ->
            Result.withDefault [] <|
                Json.Decode.decodeString decoderAttributeList str


decoderAttributeList : Decoder (List Document.Attribute)
decoderAttributeList =
    Json.Decode.oneOf
        [ Json.Decode.null []
        , Json.Decode.list <|
            Json.Decode.map4 Document.Attribute
                (Json.Decode.field "field" Json.Decode.string)
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "width" Json.Decode.int)
                (Json.Decode.field "value" (Json.Decode.maybe Json.Decode.string))
        ]


{-| Configuration object for abstracting Relay pagination functions referring to a list of documents.

It contains all the selection set functions needed for building
queries according to the Relay pagination specification.

-}
graphqlDocumentObjects : Connection.GraphqlObjects {} Graphql.Object.DocumentsConnection Graphql.Object.DocumentsEdge Graphql.Object.Document Graphql.Object.PageInfo Graphql.Scalar.Cursor Document
graphqlDocumentObjects =
    { totalCount = Graphql.Object.DocumentsConnection.totalCount
    , pageInfo = Graphql.Object.DocumentsConnection.pageInfo
    , edges = Graphql.Object.DocumentsConnection.edges
    , cursor = Graphql.Object.DocumentsEdge.cursor
    , node = Graphql.Object.DocumentsEdge.node
    , hasNextPage = Graphql.Object.PageInfo.hasNextPage
    , hasPreviousPage = Graphql.Object.PageInfo.hasPreviousPage
    }
