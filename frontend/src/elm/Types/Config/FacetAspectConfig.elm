module Types.Config.FacetAspectConfig exposing (FacetAspectConfig, aspects, get, getLabelOrAspectName)

{-| TODO
-}

import Maybe.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Localization as Localization
import Utils.List


type alias FacetAspectConfig =
    { aspect : Aspect
    , label : Localization.Translations
    }


get : Aspect -> List FacetAspectConfig -> Maybe FacetAspectConfig
get =
    Utils.List.findByMapping .aspect


getLabelOrAspectName : Localization.Language -> Aspect -> List FacetAspectConfig -> String
getLabelOrAspectName language aspect listOfFacetAspectConfigs =
    get aspect listOfFacetAspectConfigs
        |> Maybe.Extra.unwrap
            (Aspect.toString aspect)
            (\facetAspectConfig ->
                Localization.translation language facetAspectConfig.label
            )


aspects : List FacetAspectConfig -> List Aspect
aspects =
    List.map .aspect
