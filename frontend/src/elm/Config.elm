module Config exposing
    ( apiUrl, pageSize
    , facetValuesToQuery, standardFacetKeys
    )

{-| Configurable values

@docs apiUrl, pageSize
@docs facetValuesToQuery, standardFacetKeys

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


{-| -}
standardFacetKeys : List String
standardFacetKeys =
    [ "type", "subject", "origin" ]
