module Constants exposing
    ( apiUrl, graphqlOperationNamePrefix
    , incrementLimitOnLoadMore, adjustLimitOnUnlistedDocument
    , maxAttributeLengthInListingView
    )

{-| Configurable values

@docs apiUrl, graphqlOperationNamePrefix
@docs incrementLimitOnLoadMore, adjustLimitOnUnlistedDocument
@docs maxAttributeLengthInListingView

-}

import Basics.Extra


{-| Endpoint for backend's GraphQL service.
-}
apiUrl : String
apiUrl =
    "/graphql"


{-| A common prefix to use for all GraphQL operation names used by the app.

For operation names see: <https://graphql.org/learn/queries/#operation-name>

-}
graphqlOperationNamePrefix : String
graphqlOperationNamePrefix =
    "mediatumView_"


{-| -}
incrementLimitOnLoadMore : Int -> Int
incrementLimitOnLoadMore limit =
    if limit <= 10 then
        20

    else if limit <= 30 then
        50

    else if limit <= 70 then
        100

    else
        ((limit + 149) // 100) * 100


{-| -}
adjustLimitOnUnlistedDocument : Int -> Int
adjustLimitOnUnlistedDocument =
    Basics.Extra.atLeast 100


{-| -}
maxAttributeLengthInListingView : Int
maxAttributeLengthInListingView =
    200
