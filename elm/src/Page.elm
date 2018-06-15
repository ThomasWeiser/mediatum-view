module Page
    exposing
        ( Page
        , entries
        , PageResult
        , initialPageResult
        , updatePageResultFromResult
        , loadingPageResult
        )

import Graphql.Scalar
import Graphqelm.Http
import Connection


type alias Page itemModel =
    Connection.Connection Graphql.Scalar.Cursor itemModel


entries : Page itemModel -> List itemModel
entries page =
    Connection.nodes page


type alias PageResult itemModel =
    { loading : Bool
    , error : Maybe (Graphqelm.Http.Error (Page itemModel))
    , page : Maybe (Page itemModel)
    }


initialPageResult : PageResult itemModel
initialPageResult =
    { loading = False
    , error = Nothing
    , page = Nothing
    }


updatePageResultFromResult :
    Result (Graphqelm.Http.Error (Page itemModel)) (Page itemModel)
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
