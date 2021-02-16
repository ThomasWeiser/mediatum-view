module Constants exposing
    ( apiUrl, graphqlOperationNamePrefix
    , validFacetAspects, validFtsAspects
    )

{-| Configurable values

@docs apiUrl, graphqlOperationNamePrefix
@docs validFacetAspects, validFtsAspects

-}

import Types.Aspect as Aspect exposing (Aspect)


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
validFacetAspects : List Aspect
validFacetAspects =
    -- TODO: Remove!
    [ "type", "subject", "origin", "author", "person", "keywords", "year" ]
        |> List.map Aspect.fromString


{-| -}
validFtsAspects : List Aspect
validFtsAspects =
    -- TODO: Remove!
    [ "title", "type", "subject", "origin", "author", "person", "keywords", "year" ]
        |> List.map Aspect.fromString
