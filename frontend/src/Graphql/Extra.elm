module Graphql.Extra exposing
    ( StrippedError
    , errorToString
    , stripError
    )

import Graphql.Http
import Graphql.Http.GraphqlError
import Http
import Json.Decode


type StrippedError
    = HttpError Graphql.Http.HttpError
    | GraphqlError (List Graphql.Http.GraphqlError.GraphqlError)


stripError : Graphql.Http.Error decodesTo -> StrippedError
stripError graphqlHttpError =
    case graphqlHttpError of
        Graphql.Http.HttpError e ->
            HttpError e

        Graphql.Http.GraphqlError _ e ->
            GraphqlError e


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
