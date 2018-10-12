module Graphql.Extra exposing
    ( StrippedError
    , errorToString
    , stripError
    )

import Graphql.Http
import Graphql.Http.GraphqlError
import Http


type StrippedError
    = HttpError Http.Error
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
            graphqErrorToString graphqlError


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad Url: \"" ++ url ++ "\""

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus response ->
            "BadStatus: "
                ++ String.fromInt response.status.code
                ++ " ("
                ++ response.status.message
                ++ ")"

        Http.BadPayload explanation _ ->
            "BadPayload: " ++ explanation


graphqErrorToString : List Graphql.Http.GraphqlError.GraphqlError -> String
graphqErrorToString errorList =
    String.join
        ", "
        (List.map .message errorList)
