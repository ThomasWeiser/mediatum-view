module Api exposing
    ( Response, Error
    , sendQueryRequest, sendMutationRequest
    )

{-| This module and its sub-modules are responsible
for communicating to the GraphQL API of mediaTUM's backend.

The available queries and mutations are located in the modules
`Api.Queries` and `Api.Mutations`.


# Types

@docs Response, Error


# Run GraphQL Requests

@docs sendQueryRequest, sendMutationRequest

-}

import Config
import Graphql.Extra
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet exposing (SelectionSet)


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

The query itself is given as a `Graphql.SelectionSet.SelectionSet`,
see [elm-graphql](https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest/Graphql-SelectionSet).
There are functions in this module to produce these selection sets for all
relevant queries of the application.

    type Msg
        = ApiResponseToplevelFolder
            (Api.Response (List ( Folder, List Folder )))
        | ...

    initCmd : Cmd Msg
    initCmd =
        makeQueryRequest
            ApiResponseToplevelFolder
            Api.Queries.toplevelFolder

-}
sendQueryRequest :
    (Response decodesTo -> msg)
    -> SelectionSet decodesTo Graphql.Operation.RootQuery
    -> Cmd msg
sendQueryRequest tagger selectionSet =
    selectionSet
        |> Graphql.Http.queryRequest Config.apiUrl
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
        |> Graphql.Http.mutationRequest Config.apiUrl
        |> Graphql.Http.send
            (Result.mapError Graphql.Extra.stripError >> tagger)
