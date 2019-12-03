module Utils.Html exposing
    ( viewApiError
    , viewCacheError
    , viewError
    )

{-| Display errors messages using a standardized layout.

@docs viewApiError
@docs viewCacheError
@docs viewError

-}

import Cache
import Cache.Derive
import Html exposing (Html)
import Html.Attributes


{-| -}
viewApiError : Cache.ApiError -> Html msg
viewApiError error =
    viewError (Cache.apiErrorToString error)


{-| -}
viewCacheError : Cache.Derive.Error -> Html msg
viewCacheError error =
    viewError (Cache.Derive.errorToString error)


{-| -}
viewError : String -> Html msg
viewError defect =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text defect ]
