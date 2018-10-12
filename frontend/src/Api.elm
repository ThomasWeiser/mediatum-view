module Api exposing
    ( ApiError
    , Response
    , makeRequest
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
import Graphqelm.Extra
import Graphqelm.Field
import Graphqelm.Http
import Graphqelm.Operation
import Graphqelm.OptionalArgument exposing (OptionalArgument(..))
import Graphqelm.SelectionSet exposing (SelectionSet, with)
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
import Graphql.Query
import Graphql.Scalar
import Json.Decode exposing (Decoder)
import Pagination.Offset.Page
import Pagination.Relay.Connection as Connection
import Pagination.Relay.Page
import Pagination.Relay.Pagination


pageSize : Int
pageSize =
    10


type alias Response decodesTo =
    Result ApiError decodesTo


type alias ApiError =
    Graphqelm.Extra.StrippedError


makeRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphqelm.Operation.RootQuery
    -> Cmd msg
makeRequest tagger selectionSet =
    selectionSet
        |> Graphqelm.Http.queryRequest "http://localhost:5000/graphql"
        |> Graphqelm.Http.send
            (Result.mapError Graphqelm.Extra.stripError >> tagger)


queryToplevelFolder : SelectionSet (List ( Folder, List Folder )) Graphqelm.Operation.RootQuery
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


querySubfolder : FolderId -> SelectionSet (List Folder) Graphqelm.Operation.RootQuery
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
        |> with (Graphql.Object.Folder.id |> Graphqelm.Field.nonNullOrFail)
        |> with Graphql.Object.Folder.parentId
        |> with (Graphql.Object.Folder.name |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.isCollection |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.numSubfolder |> Graphqelm.Field.nonNullOrFail)


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
        |> with (Graphql.Object.Folder.id |> Graphqelm.Field.nonNullOrFail)
        |> with Graphql.Object.Folder.parentId
        |> with (Graphql.Object.Folder.name |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.isCollection |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.numSubfolder |> Graphqelm.Field.nonNullOrFail)
        |> with
            (Graphql.Object.Folder.subfolders identity
                (Graphql.Object.FoldersConnection.selection identity
                    |> with (Graphql.Object.FoldersConnection.nodes folderNode)
                )
            )


queryFolderDocuments :
    Maybe (Pagination.Relay.Page.Page Document)
    -> Pagination.Relay.Pagination.Position
    -> FolderId
    -> SelectionSet (Pagination.Relay.Page.Page Document) Graphqelm.Operation.RootQuery
queryFolderDocuments referencePage paginationPosition folderId =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.allDocuments
                ((\optionals ->
                    { optionals
                        | folderId = Present (Folder.idToInt folderId)
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
    -> FolderId
    -> String
    -> String
    -> String
    -> SelectionSet (Pagination.Offset.Page.Page FtsDocumentResult) Graphqelm.Operation.RootQuery
queryFtsPage referencePage paginationPosition folderId searchString searchDomain searchLanguage =
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
                        (Graphql.Object.Folder.ftsPage
                            (\optionals ->
                                { optionals
                                    | text = Present searchString
                                    , domain = Present searchDomain
                                    , language = Present searchLanguage
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
                            |> Graphqelm.Field.nonNullOrFail
                        )
                )
                |> Graphqelm.Field.nonNullOrFail
            )


queryFtsFolderCounts :
    FolderId
    -> String
    -> String
    -> String
    -> SelectionSet FolderCounts Graphqelm.Operation.RootQuery
queryFtsFolderCounts folderId searchString searchDomain searchLanguage =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.folderById
                (\optionals ->
                    { optionals
                        | id = Present (Folder.idToInt folderId)
                    }
                )
                (Graphql.Object.Folder.selection Dict.fromList
                    |> with
                        (Graphql.Object.Folder.ftsDocset
                            (\optionals ->
                                { optionals
                                    | text = Present searchString
                                    , domain = Present searchDomain
                                    , language = Present searchLanguage
                                }
                            )
                            (Graphql.Object.Docset.selection (::)
                                |> with
                                    (Graphql.Object.Docset.folderCount
                                        folderCount
                                        |> Graphqelm.Field.nonNullOrFail
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
                            |> Graphqelm.Field.nonNullOrFail
                        )
                )
                |> Graphqelm.Field.nonNullOrFail
            )


folderCount : SelectionSet ( FolderId, Int ) Graphql.Object.FolderCount
folderCount =
    Graphql.Object.FolderCount.selection (\a b -> ( a, b ))
        |> with
            (Graphql.Object.FolderCount.folderId
                |> Graphqelm.Field.nonNullOrFail
                |> Graphqelm.Field.map Folder.idFromInt
            )
        |> with
            (Graphql.Object.FolderCount.count
                |> Graphqelm.Field.nonNullOrFail
            )



{-
   queryAuthorSearch :
       Maybe (Pagination.Relay.Page.Page Document)
       -> Pagination.Relay.Pagination.Position
       -> FolderId
       -> String
       -> SelectionSet (Pagination.Relay.Page.Page Document) Graphqelm.Operation.RootQuery
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
                   |> Graphqelm.Field.nonNullOrFail
               )
-}


queryDocumentDetails :
    DocumentId
    -> SelectionSet (Maybe Document) Graphqelm.Operation.RootQuery
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
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResultPage.hasNextPage
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResultPage.content
                (ftsDocumentResult maskName)
                |> Graphqelm.Field.nonNullOrFail
                |> Graphqelm.Field.nonNullElementsOrFail
            )


ftsDocumentResult : String -> SelectionSet FtsDocumentResult Graphql.Object.FtsDocumentResult
ftsDocumentResult maskName =
    Graphql.Object.FtsDocumentResult.selection FtsDocumentResult.init
        |> with
            (Graphql.Object.FtsDocumentResult.number
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResult.distance
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.FtsDocumentResult.document
                (documentNode maskName)
                |> Graphqelm.Field.nonNullOrFail
            )


documentNode : String -> SelectionSet Document Graphql.Object.Document
documentNode maskName =
    Graphql.Object.Document.selection Document.init
        |> with
            (Graphql.Object.Document.id
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.Document.metadatatype
                (Graphql.Object.Metadatatype.selection identity
                    |> with
                        (Graphql.Object.Metadatatype.longname
                            |> Graphqelm.Field.nonNullOrFail
                        )
                )
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.Document.name
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Graphql.Object.Document.valuesByMask
                (\optionals ->
                    { optionals
                        | maskName = Present maskName
                    }
                )
                |> Graphqelm.Field.map mapJsonToAttributes
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
