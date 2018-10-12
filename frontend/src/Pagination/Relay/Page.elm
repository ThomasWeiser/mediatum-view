module Pagination.Relay.Page exposing
    ( Page
    , PageResult
    , entries
    , initialPageResult
    , loadingPageResult
    , updatePageResultFromResult
    )

import Graphql.Extra
import Graphql.Scalar
import Pagination.Relay.Connection as Connection exposing (Connection)


type alias Page itemModel =
    Connection Graphql.Scalar.Cursor itemModel


entries : Page itemModel -> List itemModel
entries page =
    Connection.nodes page


type alias PageResult itemModel =
    { loading : Bool
    , error : Maybe Graphql.Extra.StrippedError
    , page : Maybe (Page itemModel)
    }


initialPageResult : PageResult itemModel
initialPageResult =
    { loading = False
    , error = Nothing
    , page = Nothing
    }


updatePageResultFromResult :
    Result Graphql.Extra.StrippedError (Page itemModel)
    -> PageResult itemModel
    -> PageResult itemModel
updatePageResultFromResult result pageResult =
    case result of
        Err err ->
            { pageResult | loading = False, error = Just err }

        Ok content ->
            { loading = False, error = Nothing, page = Just content }


loadingPageResult : PageResult itemModel -> PageResult itemModel
loadingPageResult pageResult =
    { pageResult | loading = True }
