module Api exposing
    ( Response, Error
    , sendQueryRequest, sendMutationRequest
    )

{-| Definitions of all specific GraphQL requests needed in the application.


# Types

@docs Response, Error


# Run GraphQL Requests

@docs sendQueryRequest, sendMutationRequest

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
import Query exposing (Query)
import Query.Attribute


{-| Endpoint for backend's GraphQL service.
-}
apiUrl : String
apiUrl =
    "/graphql"


{-| A query specific Result type.
-}
type alias Response decodesTo =
    Result Error decodesTo


{-| Represents an error from running a GraphQL request.
-}
type alias Error =
    Graphql.Extra.StrippedError


{-| Create a GraphQL query.

Takes a tagger function for wrapping the result,
which is either a query specific type representing the queried data
or an Error.

The query itself is given as a `Graphql.SelectionSet.SelectionSet`
, see [elm-graphql](https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest/Graphql-SelectionSet).
There are functions in this module to produce these selection sets for all
relevant queries of the application.

-}
sendQueryRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootQuery
    -> Cmd msg
sendQueryRequest tagger selectionSet =
    selectionSet
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send
            (Result.mapError Graphql.Extra.stripError >> tagger)


{-| Create a GraphQL mutation.

Like `makeQueryRequest` but for mutations.

-}
sendMutationRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootMutation
    -> Cmd msg
sendMutationRequest tagger selectionSet =
    selectionSet
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send
            (Result.mapError Graphql.Extra.stripError >> tagger)
