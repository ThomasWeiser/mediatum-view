module Types.FacetValue exposing
    ( FacetValue, FacetValues, FacetsValues
    , facetsValuesFromDict
    , valueTextWithSubstitution
    )

{-|

@docs FacetValue, FacetValues, FacetsValues
@docs facetsValuesFromDict
@docs valueTextWithSubstitution

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Sort.Dict
import String.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config exposing (Config)
import Types.Localization as Localization
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


valueTextWithSubstitution : Config -> String -> Html msg
valueTextWithSubstitution config string =
    if String.Extra.isBlank string then
        Html.i []
            [ Localization.text config
                { en = "[not specified]"
                , de = "[nicht angegeben]"
                }
            ]

    else
        Html.text string
