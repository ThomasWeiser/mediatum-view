module Api exposing
    ( Response, Error
    , sendQueryRequest, sendMutationRequest, withOperationName
    , errorToString
    )

{-| This module and its sub-modules are responsible
for communicating with the GraphQL API of mediaTUM's backend.

The available queries and mutations are located in the modules
`Api.Queries` and `Api.Mutations`.


# Types

@docs Response, Error


# Run GraphQL Requests

@docs sendQueryRequest, sendMutationRequest, withOperationName


# Error handling

@docs errorToString

-}

import Config
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet exposing (SelectionSet)
import Utils.Graphql


{-| A query-specific Result type with the fixed error type [`Api.Error`](#Error)
-}
type alias Response decodesTo =
    Result Error decodesTo


{-| Represents an error from running a GraphQL request.
-}
type alias Error =
    Utils.Graphql.StrippedError


{-| -}
errorToString : Error -> String
errorToString error =
    Utils.Graphql.errorToString error


{-| Create a GraphQL query.

Takes a tagger function for wrapping the result,
which is either a query specific type representing the queried data
or an Error.

The query itself is given as a `Graphql.SelectionSet.SelectionSet`,
see [elm-graphql](https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest/Graphql-SelectionSet).
There are functions in [`Api.Queries`](Api-Queries) to produce these selection sets for all
relevant queries of the application.

    type Msg
        = ApiResponseToplevelFolder
            (Api.Response (List ( Folder, List Folder )))
        | ...

    initCmd : Cmd Msg
    initCmd =
        makeQueryRequest
            "Some operation name for poss. server-side logging and debugging"
            ApiResponseToplevelFolder
            Api.Queries.toplevelFolder

-}
sendQueryRequest :
    String
    -> (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootQuery
    -> Cmd msg
sendQueryRequest operationName tagger selectionSet =
    selectionSet
        |> Graphql.Http.queryRequest Config.apiUrl
        |> Graphql.Http.withOperationName operationName
        |> Graphql.Http.send
            (Result.mapError Utils.Graphql.stripError >> tagger)


{-| Create a GraphQL mutation.

Like `makeQueryRequest` but for mutations.

-}
sendMutationRequest :
    String
    -> (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootMutation
    -> Cmd msg
sendMutationRequest operationName tagger selectionSet =
    selectionSet
        |> Graphql.Http.mutationRequest Config.apiUrl
        |> Graphql.Http.withOperationName operationName
        |> Graphql.Http.send
            (Result.mapError Utils.Graphql.stripError >> tagger)


{-| Build a specific GraphQL operation name (see <https://graphql.org/learn/queries/#operation-name>).
-}
withOperationName : String -> String
withOperationName name =
    Config.graphqlOperationNamePrefix ++ name
