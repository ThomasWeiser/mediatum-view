module Types.Config.FtsAspect exposing (FtsAspect, get, getLabelOrAspectName)

{-| TODO Docs
-}

import Maybe.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Localization as Localization
import Utils.List


type alias FtsAspect =
    { aspect : Aspect
    , label : Localization.Translations
    }


get : Aspect -> List FtsAspect -> Maybe FtsAspect
get =
    Utils.List.findByMapping .aspect


getLabelOrAspectName : Localization.Language -> Aspect -> List FtsAspect -> String
getLabelOrAspectName language aspect ftsAspects =
    get aspect ftsAspects
        |> Maybe.Extra.unwrap
            (Aspect.toString aspect)
            (\ftsAspect ->
                Localization.translation language ftsAspect.label
            )
