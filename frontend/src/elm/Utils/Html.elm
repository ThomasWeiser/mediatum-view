module Utils.Html exposing
    ( viewApiError
    , viewCacheError
    , viewError
    , displayNone
    )

{-| Display errors messages using a standardized layout.

@docs viewApiError
@docs viewCacheError
@docs viewError

@docs displayNone

-}

import Cache
import Cache.Derive
import Html exposing (Attribute, Html)
import Html.Attributes
import Types.ApiData as ApiData exposing (ApiData, ApiError)


{-| -}
viewApiError : ApiError -> Html msg
viewApiError error =
    viewError (ApiData.apiErrorToString error)


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


{-| -}
displayNone : Bool -> Attribute msg
displayNone hide =
    if hide then
        Html.Attributes.style "display" "none"

    else
        Html.Attributes.class ""
