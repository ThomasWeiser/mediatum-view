module Types.Config.FacetAspectConfig exposing
    ( FacetAspectConfig
    , get, getLabelOrAspectName
    , aspects
    )

{-| Configuration of supported facets.

@docs FacetAspectConfig
@docs get, getLabelOrAspectName
@docs aspects

-}

import Maybe.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Localization as Localization
import Utils.List


{-| The configuration of a facet as needed for the UI.
-}
type alias FacetAspectConfig =
    { aspect : Aspect
    , label : Localization.Translations
    }


{-| Lookup a configuration of an aspect in a list
-}
get : Aspect -> List FacetAspectConfig -> Maybe FacetAspectConfig
get =
    Utils.List.findByMapping .aspect


{-| Get the label of the facet as per configuration, using the current language.

If the given aspect has no configuration (should never happen) then we use the raw aspect name.

-}
getLabelOrAspectName : { c | uiLanguage : Localization.Language } -> Aspect -> List FacetAspectConfig -> String
getLabelOrAspectName config aspect listOfFacetAspectConfigs =
    get aspect listOfFacetAspectConfigs
        |> Maybe.Extra.unwrap
            (Aspect.toString aspect)
            (\facetAspectConfig ->
                Localization.string config facetAspectConfig.label
            )


{-| Extract the raw aspects
-}
aspects : List FacetAspectConfig -> List Aspect
aspects =
    List.map .aspect
