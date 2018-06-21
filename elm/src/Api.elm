module Api
    exposing
        ( Response
        , makeRequest
        , queryToplevelFolder
        , querySubfolder
        , querySimpleSearch
        , queryAuthorSearch
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
import Connection
import Pagination
import Page exposing (Page)
import Folder exposing (FolderId, Folder)
import Document exposing (Document, Attribute)


pageSize : Int
pageSize =
    10


sizeLimitSimpleSearch : Int
sizeLimitSimpleSearch =
    100


type alias Response decodesTo =
    Result (Graphqelm.Http.Error decodesTo) decodesTo


makeRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphqelm.Operation.RootQuery
    -> Cmd msg
makeRequest tagger selectionSet =
    selectionSet
        |> Graphqelm.Http.queryRequest "http://localhost:5000/graphql"
        |> Graphqelm.Http.send tagger


queryToplevelFolder : SelectionSet (List Folder) Graphqelm.Operation.RootQuery
queryToplevelFolder =
    Graphql.Query.selection Maybe.Extra.values
        |> with
            (Graphql.Query.allFolders
                (\optionals ->
                    { optionals
                        | isToplevel = Present True
                    }
                )
                (Graphql.Object.FoldersConnection.selection identity
                    |> with (Graphql.Object.FoldersConnection.nodes folderNode)
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
        |> with (Graphql.Object.Folder.parentId |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.name |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.isToplevel |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.isCollection |> Graphqelm.Field.nonNullOrFail)
        |> with (Graphql.Object.Folder.numSubfolder |> Graphqelm.Field.nonNullOrFail)


querySimpleSearch :
    Maybe (Page Document)
    -> Pagination.Position
    -> String
    -> List String
    -> SelectionSet (Page Document) Graphqelm.Operation.RootQuery
querySimpleSearch referencePage paginationPosition searchString searchDomains =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.simpleSearch
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
                    documentNode
                )
            )


queryAuthorSearch :
    Maybe (Page Document)
    -> Pagination.Position
    -> String
    -> SelectionSet (Page Document) Graphqelm.Operation.RootQuery
queryAuthorSearch referencePage paginationPosition searchString =
    Graphql.Query.selection identity
        |> with
            (Graphql.Query.authorSearch
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
                    documentNode
                )
            )


documentNode : SelectionSet Document Graphql.Object.Document
documentNode =
    Graphql.Object.Document.selection Document
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
                        | maskName = Present "nodesmall"
                    }
                )
                |> Graphqelm.Field.map mapJsonToAttributes
            )


mapJsonToAttributes : Maybe Graphql.Scalar.Json -> List Attribute
mapJsonToAttributes maybeJson =
    case maybeJson of
        Nothing ->
            []

        Just (Graphql.Scalar.Json str) ->
            Result.withDefault [] <|
                Json.Decode.decodeString decoderAttributeList str


decoderAttributeList : Decoder (List Attribute)
decoderAttributeList =
    Json.Decode.oneOf
        [ Json.Decode.null []
        , Json.Decode.list <|
            Json.Decode.map4 Attribute
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
