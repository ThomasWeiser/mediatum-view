module Utils.Html exposing
    ( viewApiError
    , viewCacheError
    , viewError
    )

import Api
import Cache
import Html exposing (Html)
import Html.Attributes


viewApiError : Api.Error -> Html msg
viewApiError error =
    viewError (Api.errorToString error)


viewCacheError : Cache.Error -> Html msg
viewCacheError error =
    viewError (Cache.errorToString error)


viewError : String -> Html msg
viewError defect =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text defect ]
