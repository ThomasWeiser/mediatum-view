module Config exposing (apiUrl, pageSize)

{-| Configurable values

@docs apiUrl, pageSize

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
