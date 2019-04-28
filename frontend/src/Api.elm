module Api exposing
    ( ApiError
    , Response
    , makeMutationRequest
    , makeQueryRequest
    , queryDocumentDetails
    , queryFolderDocumentsPage
    , queryFolderFolderCounts
    , queryFtsFolderCounts
    , queryFtsPage
    , queryGenericNode
    , querySubfolder
    , queryToplevelFolder
    , updateDocumentAttribute
    )

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


apiUrl : String
apiUrl =
    "/graphql"


pageSize : Int
pageSize =
    10


type alias Response decodesTo =
    Result ApiError decodesTo


type alias ApiError =
    Graphql.Extra.StrippedError


makeQueryRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootQuery
    -> Cmd msg
makeQueryRequest tagger selectionSet =
    selectionSet
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send
            (Result.mapError Graphql.Extra.stripError >> tagger)


makeMutationRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootMutation
    -> Cmd msg
makeMutationRequest tagger selectionSet =
    selectionSet
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send
            (Result.mapError Graphql.Extra.stripError >> tagger)


queryToplevelFolder : SelectionSet (List ( Folder, List Folder )) Graphql.Operation.RootQuery
queryToplevelFolder =
    Graphql.Query.allFolders
        (\optionals ->
            { optionals
                | isRoot = Present True
            }
        )
        (Graphql.Object.FoldersConnection.nodes folderNodeWithSubfolders)


querySubfolder : List FolderId -> SelectionSet (List Folder) Graphql.Operation.RootQuery
querySubfolder folderIds =
    Graphql.Query.allFolders
        (\optionals ->
            { optionals
                | parentIds = List.map (Folder.idToInt >> Just) folderIds |> Present
            }
        )
        (Graphql.Object.FoldersConnection.nodes folderNode)


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


folderNode : SelectionSet Folder Graphql.Object.Folder
folderNode =
    SelectionSet.succeed Folder.init
        |> SelectionSet.with (Graphql.Object.Folder.id |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with Graphql.Object.Folder.parentId
        |> SelectionSet.with (Graphql.Object.Folder.name |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with (Graphql.Object.Folder.isCollection |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with (Graphql.Object.Folder.numSubfolder |> SelectionSet.nonNullOrFail)


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
