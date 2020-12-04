module Types.Facet exposing (FacetValue, FacetValues)

{-|

@docs FacetValue, FacetValues

-}


{-| A facet value is the value of an attribute and a count
of all occurences of this attribute value in a set of documents.
-}
type alias FacetValue =
    { value : String
    , count : Int
    }


{-| A list of facet values for a specific aspect or metadata field.
-}
type alias FacetValues =
    -- Poss. "FacetFan" ?
    List FacetValue
