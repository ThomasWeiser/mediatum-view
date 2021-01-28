module Types.Facet exposing
    ( FacetValue, FacetValues, FacetsValues
    , facetsValuesFromDict
    )

{-|

@docs FacetValue, FacetValues, FacetsValues
@docs facetsValuesFromDict

-}

import Dict exposing (Dict)
import Sort.Dict
import Types.Aspect as Aspect exposing (Aspect)
import Utils


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


{-| A collection of facets, each with a list of facet values.
-}
type alias FacetsValues =
    Sort.Dict.Dict Aspect FacetValues


{-| Construct from a simple dict coming from API
-}
facetsValuesFromDict : Dict String FacetValues -> FacetsValues
facetsValuesFromDict dict =
    dict
        |> Dict.toList
        |> List.map (Tuple.mapFirst Aspect.fromString)
        |> Sort.Dict.fromList (Utils.sorter Aspect.ordering)
