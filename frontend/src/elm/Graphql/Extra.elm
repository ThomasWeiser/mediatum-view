module Graphql.Extra exposing (StrippedError, stripError, errorToString)

{-| Provide a simplified error type for GraphQL operations.

The package `dillonkearns/elm-graphql` provides its own
[error type](https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest/Graphql-Http#Error)
`Graphql.Http.Error`,
which is paramterized on the type that the query should decode to.

For ease of use we define our own error type, which strips out this type parameterization and
the respective sub-fields. This simplified uniform error reporting.


# Error Representation

@docs StrippedError, stripError, errorToString

-}

import Graphql.Http
import Graphql.Http.GraphqlError
import Json.Decode


{-| Simplified error type.

Like Graphql.Http.Error, but without the `(PossiblyParsedData parsedData)` part.
So it is not dependent of the type parameter `parsedData`.

-}
type StrippedError
    = HttpError Graphql.Http.HttpError
    | GraphqlError (List Graphql.Http.GraphqlError.GraphqlError)


{-| Convert an Graphql.Http.Error to a StrippedError.
-}
stripError : Graphql.Http.Error decodesTo -> StrippedError
stripError graphqlHttpError =
    case graphqlHttpError of
        Graphql.Http.HttpError e ->
            HttpError e

        Graphql.Http.GraphqlError _ e ->
            GraphqlError e


{-| Render some suitable explanation text for an error.
-}
errorToString : StrippedError -> String
errorToString error =
    case error of
        HttpError httpError ->
            httpErrorToString httpError

        GraphqlError graphqlError ->
            graphqlErrorToString graphqlError


httpErrorToString : Graphql.Http.HttpError -> String
httpErrorToString error =
    case error of
        Graphql.Http.BadUrl url ->
            "Bad Url: \"" ++ url ++ "\""

        Graphql.Http.Timeout ->
            "Timeout"

        Graphql.Http.NetworkError ->
            "NetworkError"

        Graphql.Http.BadStatus metadata payload ->
            "BadStatus: "
                ++ String.fromInt metadata.statusCode
                ++ " ("
                ++ metadata.statusText
                ++ ")"

        Graphql.Http.BadPayload jsonDecodeError ->
            "BadPayload: " ++ Json.Decode.errorToString jsonDecodeError


graphqlErrorToString : List Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString errorList =
    String.join
        ", "
        (List.map .message errorList)
