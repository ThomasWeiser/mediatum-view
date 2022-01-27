module Constants exposing
    ( apiUrl, graphqlOperationNamePrefix
    , incrementLimitOnLoadMore
    , maxAttributeLengthInListingView
    , externalServerUrls
    )

{-| Configurable values

@docs apiUrl, graphqlOperationNamePrefix
@docs incrementLimitOnLoadMore
@docs maxAttributeLengthInListingView
@docs contentServerUrls

-}

import Types.Id as Id


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
maxAttributeLengthInListingView : Int
maxAttributeLengthInListingView =
    200


{-| Content like document files and thumbnails are provided by a separate server.

This value is a record of URL building functions to this server.

-}
externalServerUrls :
    { thumbnail : Id.DocumentId -> String
    , presentation : Id.DocumentId -> String
    , item : String -> String
    }
externalServerUrls =
    let
        appendId base id =
            base ++ Id.toString id
    in
    { thumbnail = "https://mediatum.ub.tum.de/thumbs/" |> appendId
    , presentation = "https://mediatum.ub.tum.de/thumb2/" |> appendId
    , item = \itemSpec -> "https://mediatum.ub.tum.de/?item=" ++ itemSpec ++ ".html"
    }
