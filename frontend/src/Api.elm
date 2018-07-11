module Api
    exposing
        ( Response
        , ApiError
        , makeRequest
        , queryToplevelFolder
        , querySubfolder
        , queryFolderDocuments
        , querySimpleSearch
        , queryAuthorSearch
        , queryDocumentDetails
        , sizeLimitSimpleSearch
        )

import Graphql.Object
import Graphql.Object.PageInfo
import Graphql.Object.FoldersConnection
import Graphql.Object.Folder
import Graphql.Object.DocumentsConnection
import Graphql.Object.DocumentsEdge
import Graphql.Object.Document
import Graphql.Object.Metadatatype
import Graphql.Query
import Graphql.Scalar
import Json.Decode exposing (Decoder)
import Maybe.Extra
import Graphqelm.Field
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Graphqelm.SelectionSet exposing (SelectionSet, with, hardcoded)
import Graphqelm.Http
import Graphqelm.Operation
import Graphqelm.Extra
import Connection
import Pagination
import Page exposing (Page)
import Folder exposing (FolderId, Folder)
import Document exposing (Document)


pageSize : Int
pageSize =
    10


sizeLimitSimpleSearch : Int
sizeLimitSimpleSearch =
    100


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
    Graphql.Query.selection Maybe.Extra.values
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
    Graphql.Query.selection Maybe.Extra.values
        |> with
            (Graphql.Query.allFolders
                (\optionals ->
                    { optionals
                        | parentId = Present (Folder.idAsInt folderId)
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
        |> with (Graphql.Object.Folder.parentId)
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
            |> with (Graphql.Object.Folder.parentId)
            |> with (Graphql.Object.Folder.name |> Graphqelm.Field.nonNullOrFail)
            |> with (Graphql.Object.Folder.isCollection |> Graphqelm.Field.nonNullOrFail)
            |> with (Graphql.Object.Folder.numSubfolder |> Graphqelm.Field.nonNullOrFail)
            |> with
                (Graphql.Object.Folder.subfolders identity
                    (Graphql.Object.FoldersConnection.selection Maybe.Extra.values
                        |> with (Graphql.Object.FoldersConnection.nodes folderNode)
                    )
                )


queryFolderDocuments :
    Maybe (Page Document)
    -> Pagination.Position
    -> FolderId
    -> SelectionSet (Page Document) Graphqelm.Operation.RootQuery
queryFolderDocuments referencePage paginationPosition folderId =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.folderById
                (\optionals ->
                    { optionals
                        | id = Present (Folder.idAsInt folderId)
                    }
                )
                (Graphql.Object.Folder.selection identity
                    |> with
                        (Graphql.Object.Folder.documents
                            (Pagination.paginationArguments
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


querySimpleSearch :
    Maybe (Page Document)
    -> Pagination.Position
    -> FolderId
    -> String
    -> List String
    -> SelectionSet (Page Document) Graphqelm.Operation.RootQuery
querySimpleSearch referencePage paginationPosition folderId searchString searchDomains =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.folderById
                (\optionals ->
                    { optionals
                        | id = Present (Folder.idAsInt folderId)
                    }
                )
                (Graphql.Object.Folder.selection identity
                    |> with
                        (Graphql.Object.Folder.simpleSearch
                            ((\optionals ->
                                { optionals
                                    | text = Present searchString
                                    , domains = Present (List.map Just searchDomains)
                                    , limit = Present sizeLimitSimpleSearch
                                }
                             )
                                >> Pagination.paginationArguments
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


queryAuthorSearch :
    Maybe (Page Document)
    -> Pagination.Position
    -> FolderId
    -> String
    -> SelectionSet (Page Document) Graphqelm.Operation.RootQuery
queryAuthorSearch referencePage paginationPosition folderId searchString =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.folderById
                (\optionals ->
                    { optionals
                        | id = Present (Folder.idAsInt folderId)
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
                                >> Pagination.paginationArguments
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


queryDocumentDetails :
    Int
    -> SelectionSet (Maybe Document) Graphqelm.Operation.RootQuery
queryDocumentDetails documentId =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.documentById
                (\optionals ->
                    { optionals
                        | id = Present documentId
                    }
                )
                (documentNode "nodebig")
            )


documentNode : String -> SelectionSet Document Graphql.Object.Document
documentNode maskName =
    Graphql.Object.Document.selection Document
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
