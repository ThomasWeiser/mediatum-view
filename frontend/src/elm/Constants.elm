module Constants exposing
    ( apiUrl, graphqlOperationNamePrefix
    , incrementLimitOnShowMore
    )

{-| Configurable values

@docs apiUrl, graphqlOperationNamePrefix
@docs incrementLimitOnShowMore

-}


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
incrementLimitOnShowMore : Int -> Int
incrementLimitOnShowMore limit =
    if limit <= 10 then
        20

    else if limit <= 30 then
        50

    else if limit <= 70 then
        100

    else
        ((limit + 149) // 100) * 100
