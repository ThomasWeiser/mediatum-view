module Config exposing
    ( apiUrl, pageSize, facetValuesToQuery
    , standardFacetKey
    )

{-| Configurable values

@docs apiUrl, pageSize, facetValuesToQuery

-}


{-| Endpoint for backend's GraphQL service.
-}
apiUrl : String
apiUrl =
    "/graphql"


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


standardFacetKey : String
standardFacetKey =
    "subject"
