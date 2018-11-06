module Api exposing
    ( ApiError
    , Response
    , makeQueryRequest
    , queryDocumentDetails
    , queryFolderDocuments
    , queryFtsFolderCounts
    , queryFtsPage
    , querySubfolder
    , queryToplevelFolder
    )

import Dict
import Document exposing (Document, DocumentId)
import Folder exposing (Folder, FolderCounts, FolderId)
import FtsDocumentResult exposing (FtsDocumentResult)
import Graphql.Extra
import Graphql.Field
import Graphql.Http
import Graphql.Object
import Graphql.Object.Docset
import Graphql.Object.Document
import Graphql.Object.DocumentsConnection
import Graphql.Object.DocumentsEdge
import Graphql.Object.Folder
import Graphql.Object.FolderCount
import Graphql.Object.FolderCountsConnection
import Graphql.Object.FoldersConnection
import Graphql.Object.FtsDocumentResult
import Graphql.Object.FtsDocumentResultPage
import Graphql.Object.Metadatatype
import Graphql.Object.PageInfo
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Query
import Graphql.Scalar
import Graphql.SelectionSet exposing (SelectionSet, with)
import Json.Decode exposing (Decoder)
import Pagination.Offset.Page
import Pagination.Relay.Connection as Connection
import Pagination.Relay.Page
import Pagination.Relay.Pagination
import Query exposing (Query)
import Query.Attribute


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
        |> Graphql.Http.queryRequest "http://localhost:5000/graphql"
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


querySubfolder : FolderId -> SelectionSet (List Folder) Graphql.Operation.RootQuery
querySubfolder folderId =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.allFolders
                (\optionals ->
                    { optionals
                        | parentId = Present (Folder.idToInt folderId)
                    }
                )
                (Graphql.Object.FoldersConnection.selection identity
                    |> with (Graphql.Object.FoldersConnection.nodes folderNode)
                )
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


queryFolderDocuments :
    Maybe (Pagination.Relay.Page.Page Document)
    -> Pagination.Relay.Pagination.Position
    -> Query.FolderQuery
    -> SelectionSet (Pagination.Relay.Page.Page Document) Graphql.Operation.RootQuery
queryFolderDocuments referencePage paginationPosition folderQuery =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.allDocuments
                ((\optionals ->
                    { optionals
                        | folderId = folderQuery.folder |> .id |> Folder.idToInt |> Present
                        , attributeTests =
                            folderQuery.filters
                                |> Query.filtersToAttributeTests
                                |> Query.Attribute.testsAsGraphqlArgument
                                |> Present
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


queryFtsPage :
    Maybe (Pagination.Offset.Page.Page FtsDocumentResult)
    -> Pagination.Offset.Page.Position
    -> Query.FtsQuery
    -> SelectionSet (Pagination.Offset.Page.Page FtsDocumentResult) Graphql.Operation.RootQuery
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
                            (ftsDocumentResultPage "nodesmall")
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
                (Graphql.Object.Folder.selection Dict.fromList
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
                            (Graphql.Object.Docset.selection (::)
                                |> with
                                    (Graphql.Object.Docset.folderCount
                                        folderCount
                                        |> Graphql.Field.nonNullOrFail
                                    )
                                |> with
                                    (Graphql.Object.Docset.subfolderCounts
                                        identity
                                        (Graphql.Object.FolderCountsConnection.selection identity
                                            |> with
                                                (Graphql.Object.FolderCountsConnection.nodes
                                                    folderCount
                                                )
                                        )
                                    )
                            )
                            |> Graphql.Field.nonNullOrFail
                        )
                )
                |> Graphql.Field.nonNullOrFail
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


ftsDocumentResultPage :
    String
    -> SelectionSet (Pagination.Offset.Page.Page FtsDocumentResult) Graphql.Object.FtsDocumentResultPage
ftsDocumentResultPage maskName =
    Graphql.Object.FtsDocumentResultPage.selection Pagination.Offset.Page.Page
        |> with
            (Graphql.Object.FtsDocumentResultPage.offset
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResultPage.hasNextPage
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResultPage.content
                (ftsDocumentResult maskName)
                |> Graphql.Field.nonNullOrFail
                |> Graphql.Field.nonNullElementsOrFail
            )


ftsDocumentResult : String -> SelectionSet FtsDocumentResult Graphql.Object.FtsDocumentResult
ftsDocumentResult maskName =
    Graphql.Object.FtsDocumentResult.selection FtsDocumentResult.init
        |> with
            (Graphql.Object.FtsDocumentResult.number
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResult.distance
                |> Graphql.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResult.document
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
