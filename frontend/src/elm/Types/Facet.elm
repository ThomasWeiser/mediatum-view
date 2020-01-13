module Types.Facet exposing (FacetValue, FacetValues)

{-| -}


{-| -}
type alias FacetValue =
    { value : String
    , count : Int
    }


type alias FacetValues =
    -- Poss. "FacetFan" ?
    List FacetValue
