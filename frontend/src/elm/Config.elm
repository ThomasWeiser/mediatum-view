module Config exposing
    ( apiUrl, graphqlOperationNamePrefix
    , pageSize
    , facetValuesToQuery, standardFacetAspects
    )

{-| Configurable values

@docs apiUrl, graphqlOperationNamePrefix
@docs pageSize
@docs facetValuesToQuery, standardFacetAspects

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


{-| Number of results per page used for pagination.
Currently hard-coded.
-}
pageSize : Int
pageSize =
    10


{-| Number of results per page used for pagination.
Currently hard-coded.
-}
facetValuesToQuery : Int
facetValuesToQuery =
    20


{-| -}
standardFacetAspects : List String
standardFacetAspects =
    [ "type", "subject", "origin", "author", "person", "keywords", "year" ]
