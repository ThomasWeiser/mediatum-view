module Graphql.Extra exposing
    ( StrippedError
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
