module Graphqelm.Extra exposing
    ( StrippedError
    , stripError
    )

import Graphqelm.Http
import Graphqelm.Http.GraphqlError
import Http


type StrippedError
    = HttpError Http.Error
    | GraphqlError (List Graphqelm.Http.GraphqlError.GraphqlError)


stripError : Graphqelm.Http.Error decodesTo -> StrippedError
stripError graphqelmHttpError =
    case graphqelmHttpError of
        Graphqelm.Http.HttpError e ->
            HttpError e

        Graphqelm.Http.GraphqlError _ e ->
            GraphqlError e
