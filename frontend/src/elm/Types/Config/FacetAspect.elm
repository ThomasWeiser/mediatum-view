module Types.Config.FacetAspect exposing (FacetAspect, aspects, get, getLabelOrAspectName)

{-| TODO
-}

import Maybe.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Localization as Localization
import Utils.List


type alias FacetAspect =
    { aspect : Aspect
    , label : Localization.Translations
    }


get : Aspect -> List FacetAspect -> Maybe FacetAspect
get =
    Utils.List.findByMapping .aspect


getLabelOrAspectName : Localization.Language -> Aspect -> List FacetAspect -> String
getLabelOrAspectName language aspect facetAspects =
    get aspect facetAspects
        |> Maybe.Extra.unwrap
            (Aspect.toString aspect)
            (\facetAspect ->
                Localization.translation language facetAspect.label
            )


aspects : List FacetAspect -> List Aspect
aspects =
    List.map .aspect
