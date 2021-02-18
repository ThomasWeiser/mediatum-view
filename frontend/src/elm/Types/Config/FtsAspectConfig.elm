module Types.Config.FtsAspectConfig exposing (FtsAspectConfig, aspects, get, getLabelOrAspectName)

{-| TODO Docs
-}

import Maybe.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Localization as Localization
import Utils.List


type alias FtsAspectConfig =
    { aspect : Aspect
    , label : Localization.Translations
    }


get : Aspect -> List FtsAspectConfig -> Maybe FtsAspectConfig
get =
    Utils.List.findByMapping .aspect


getLabelOrAspectName : Localization.Language -> Aspect -> List FtsAspectConfig -> String
getLabelOrAspectName language aspect listOfFtsAspectConfigs =
    get aspect listOfFtsAspectConfigs
        |> Maybe.Extra.unwrap
            (Aspect.toString aspect)
            (\ftsAspectConfig ->
                Localization.translation language ftsAspectConfig.label
            )


aspects : List FtsAspectConfig -> List Aspect
aspects =
    List.map .aspect
