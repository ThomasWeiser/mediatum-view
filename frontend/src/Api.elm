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

import Graphql.Extra
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet exposing (SelectionSet)


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
