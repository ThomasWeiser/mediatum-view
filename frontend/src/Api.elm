module Api exposing
    ( ApiError
    , Response
    , makeMutationRequest
    , makeQueryRequest
    , queryDocumentDetails
    , queryFolderDocumentsPage
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
import Graphql.Field
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
import Graphql.SelectionSet exposing (SelectionSet, with)
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
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.allFolders
                (\optionals ->
                    { optionals
                        | isRoot = Present True
                    }
                )
                (Graphql.Object.FoldersConnection.selection identity
                    |> with (Graphql.Object.FoldersConnection.nodes folderNodeWithSubfolders)
                )
            )


querySubfolder : List FolderId -> SelectionSet (List Folder) Graphql.Operation.RootQuery
querySubfolder folderIds =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.allFolders
                (\optionals ->
                    { optionals
                        | parentIds = Present <| List.map (Folder.idToInt >> Just) folderIds
                    }
                )
                (Graphql.Object.FoldersConnection.selection identity
                    |> with (Graphql.Object.FoldersConnection.nodes folderNode)
                )
            )


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
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.genericNodeById
                (\optionals ->
                    { optionals
                        | id = Present nodeId
                    }
                )
                (Graphql.Object.GenericNode.selection constructor
                    |> with (Graphql.Object.GenericNode.asFolder folderLineage)
                    |> with (Graphql.Object.GenericNode.asDocument (documentNode "nodebig"))
                )
                |> Graphql.Field.nonNullOrFail
            )


folderNode : SelectionSet Folder Graphql.Object.Folder
folderNode =
    Graphql.Object.Folder.selection Folder.init
        |> with (Graphql.Object.Folder.id |> Graphql.Field.nonNullOrFail)
        |> with Graphql.Object.Folder.parentId
        |> with (Graphql.Object.Folder.name |> Graphql.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.isCollection |> Graphql.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.numSubfolder |> Graphql.Field.nonNullOrFail)


folderNodeWithSubfolders : SelectionSet ( Folder, List Folder ) Graphql.Object.Folder
folderNodeWithSubfolders =
    let
        constructor : Int -> Maybe Int -> String -> Bool -> Int -> List Folder -> ( Folder, List Folder )
        constructor idAsInt maybeParentIdAsInt name isCollection numSubfolder subfolder =
            ( Folder.init idAsInt maybeParentIdAsInt name isCollection numSubfolder
            , subfolder
            )
    in
    Graphql.Object.Folder.selection constructor
        |> with (Graphql.Object.Folder.id |> Graphql.Field.nonNullOrFail)
        |> with Graphql.Object.Folder.parentId
        |> with (Graphql.Object.Folder.name |> Graphql.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.isCollection |> Graphql.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.numSubfolder |> Graphql.Field.nonNullOrFail)
        |> with
            (Graphql.Object.Folder.subfolders identity
                (Graphql.Object.FoldersConnection.selection identity
                    |> with (Graphql.Object.FoldersConnection.nodes folderNode)
                )
            )


folderLineage : SelectionSet (Nonempty Folder) Graphql.Object.Folder
folderLineage =
    Graphql.Object.Folder.selection identity
        |> with
            (Graphql.Object.Folder.lineage
                folderNode
                |> Graphql.Field.nonNullOrFail
                |> Graphql.Field.nonNullElementsOrFail
                |> Graphql.Field.mapOrFail
                    (List.Nonempty.fromList
                        >> Result.fromMaybe "Lineage needs at least one folder"
                    )
            )


queryFolderDocumentsPage :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FolderQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
queryFolderDocumentsPage referencePage paginationPosition folderQuery =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.allDocumentsPage
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
                            Present <|
                                Pagination.Offset.Page.positionToOffset
                                    pageSize
                                    referencePage
                                    paginationPosition
                    }
                )
                (documentResultPage "nodesmall")
                |> Graphql.Field.nonNullOrFail
            )


queryFtsPage :
    Maybe (Pagination.Offset.Page.Page DocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FtsQuery
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Operation.RootQuery
queryFtsPage referencePage paginationPosition ftsQuery =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.folderById
                (\optionals ->
                    { optionals
                        | id = ftsQuery.folder |> .id |> Folder.idToInt |> Present
                    }
                )
                (Graphql.Object.Folder.selection identity
                    |> with
                        (Graphql.Object.Folder.ftsPage
                            (\optionals ->
                                { optionals
                                    | text = Present ftsQuery.searchTerm
                                    , domain = Present (Query.ftsOptionsDomainToString ftsQuery.options)
                                    , language = Present (Query.ftsOptionsLanguageToString ftsQuery.options)
                                    , attributeTests =
                                        ftsQuery.filters
                                            |> Query.filtersToAttributeTests
                                            |> Query.Attribute.testsAsGraphqlArgument
                                            |> Present
                                    , limit = Present pageSize
                                    , offset =
                                        Present <|
                                            Pagination.Offset.Page.positionToOffset
                                                pageSize
                                                referencePage
                                                paginationPosition
                                }
                            )
                            (documentResultPage "nodesmall")
                            |> Graphql.Field.nonNullOrFail
                        )
                )
                |> Graphql.Field.nonNullOrFail
            )


queryFtsFolderCounts :
    Query.FtsQuery
    -> SelectionSet FolderCounts Graphql.Operation.RootQuery
queryFtsFolderCounts ftsQuery =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.folderById
                (\optionals ->
                    { optionals
                        | id = ftsQuery.folder |> .id |> Folder.idToInt |> Present
                    }
                )
                (Graphql.Object.Folder.selection identity
                    |> with
                        (Graphql.Object.Folder.ftsDocset
                            (\optionals ->
                                { optionals
                                    | text = Present ftsQuery.searchTerm
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
                            |> Graphql.Field.nonNullOrFail
                        )
                )
                |> Graphql.Field.nonNullOrFail
            )


folderAndSubfolderCounts : SelectionSet Folder.FolderCounts Graphql.Object.Docset
folderAndSubfolderCounts =
    Graphql.Object.Docset.selection
        (\pair listOfPairs -> Dict.fromList (pair :: listOfPairs))
        |> with
            (Graphql.Object.Docset.folderCount
                folderCount
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.Docset.subfolderCounts
                identity
                (Graphql.Object.FolderCountsConnection.selection
                    identity
                    |> with
                        (Graphql.Object.FolderCountsConnection.nodes
                            folderCount
                        )
                )
            )


folderCount : SelectionSet ( FolderId, Int ) Graphql.Object.FolderCount
folderCount =
    Graphql.Object.FolderCount.selection (\a b -> ( a, b ))
        |> with
            (Graphql.Object.FolderCount.folderId
                |> Graphql.Field.nonNullOrFail
                |> Graphql.Field.map Folder.idFromInt
            )
        |> with
            (Graphql.Object.FolderCount.count
                |> Graphql.Field.nonNullOrFail
            )



{-
   queryAuthorSearch :
       Maybe (Pagination.Relay.Page.Page Document)
       -> Pagination.Relay.Pagination.Position
       -> FolderId
       -> String
       -> SelectionSet (Pagination.Relay.Page.Page Document) Graphql.Operation.RootQuery
   queryAuthorSearch referencePage paginationPosition folderId searchString =
       Graphql.Query.selection identity
           |> with
               (Graphql.Query.folderById
                   (\optionals ->
                       { optionals
                           | id = Present (Folder.idToInt folderId)
                       }
                   )
                   (Graphql.Object.Folder.selection identity
                       |> with
                           (Graphql.Object.Folder.authorSearch
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
                           )
                   )
                   |> Graphql.Field.nonNullOrFail
               )
-}


queryDocumentDetails :
    DocumentId
    -> SelectionSet (Maybe Document) Graphql.Operation.RootQuery
queryDocumentDetails documentId =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.documentById
                (\optionals ->
                    { optionals
                        | id = Present (Document.idToInt documentId)
                    }
                )
                (documentNode "nodebig")
            )


updateDocumentAttribute :
    DocumentId
    -> String
    -> String
    -> SelectionSet (Maybe Document) Graphql.Operation.RootMutation
updateDocumentAttribute documentId key value =
    Graphql.Mutation.selection Maybe.Extra.join
        |> with
            (Graphql.Mutation.updateDocumentAttribute
                { input =
                    { clientMutationId = Absent
                    , id = Present (Document.idToInt documentId)
                    , key = Present key
                    , value = Present value
                    }
                }
                (Graphql.Object.UpdateDocumentAttributePayload.selection identity
                    |> with
                        (Graphql.Object.UpdateDocumentAttributePayload.document
                            (documentNode "nodebig")
                        )
                )
            )


documentResultPage :
    String
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Object.DocumentResultPage
documentResultPage maskName =
    Graphql.Object.DocumentResultPage.selection Pagination.Offset.Page.Page
        |> with
            (Graphql.Object.DocumentResultPage.offset
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.DocumentResultPage.hasNextPage
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.DocumentResultPage.content
                (documentResult maskName)
                |> Graphql.Field.nonNullOrFail
                |> Graphql.Field.nonNullElementsOrFail
            )


documentResult : String -> SelectionSet DocumentResult Graphql.Object.DocumentResult
documentResult maskName =
    Graphql.Object.DocumentResult.selection DocumentResult.init
        |> with
            (Graphql.Object.DocumentResult.number
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.DocumentResult.distance
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.DocumentResult.document
                (documentNode maskName)
                |> Graphql.Field.nonNullOrFail
            )


documentNode : String -> SelectionSet Document Graphql.Object.Document
documentNode maskName =
    Graphql.Object.Document.selection Document.init
        |> with
            (Graphql.Object.Document.id
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.Document.metadatatype
                (Graphql.Object.Metadatatype.selection identity
                    |> with
                        (Graphql.Object.Metadatatype.longname
                            |> Graphql.Field.nonNullOrFail
                        )
                )
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.Document.name
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.Document.valuesByMask
                (\optionals ->
                    { optionals
                        | maskName = Present maskName
                    }
                )
                |> Graphql.Field.map mapJsonToAttributes
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
    { connectionSelection = Graphql.Object.DocumentsConnection.selection
    , totalCount = Graphql.Object.DocumentsConnection.totalCount
    , pageInfo = Graphql.Object.DocumentsConnection.pageInfo
    , edges = Graphql.Object.DocumentsConnection.edges
    , edgeSelection = Graphql.Object.DocumentsEdge.selection
    , cursor = Graphql.Object.DocumentsEdge.cursor
    , node = Graphql.Object.DocumentsEdge.node
    , pageInfoSelection = Graphql.Object.PageInfo.selection
    , hasNextPage = Graphql.Object.PageInfo.hasNextPage
    , hasPreviousPage = Graphql.Object.PageInfo.hasPreviousPage
    }
