module Types.Config.FtsAspectConfig exposing
    ( FtsAspectConfig
    , get, getLabelOrAspectName
    , aspects
    )

{-| Configuration of supported aspects for fts.

@docs FtsAspectConfig
@docs get, getLabelOrAspectName
@docs aspects

-}

import Maybe.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Localization as Localization
import Utils.List


{-| The configuration of a fts-aspect as needed for the UI.
-}
type alias FtsAspectConfig =
    { aspect : Aspect
    , label : Localization.Translations
    }


{-| Lookup a configuration of an aspect in a list
-}
get : Aspect -> List FtsAspectConfig -> Maybe FtsAspectConfig
get =
    Utils.List.findByMapping .aspect


{-| Get the label of the fts-aspect as per configuration, using the current language.

If the given aspect has no configuration (should never happen) then we use the raw aspect name.

-}
getLabelOrAspectName : Localization.Language -> Aspect -> List FtsAspectConfig -> String
getLabelOrAspectName language aspect listOfFtsAspectConfigs =
    get aspect listOfFtsAspectConfigs
        |> Maybe.Extra.unwrap
            (Aspect.toString aspect)
            (\ftsAspectConfig ->
                Localization.translation language ftsAspectConfig.label
            )


{-| Extract the raw aspects
-}
aspects : List FtsAspectConfig -> List Aspect
aspects =
    List.map .aspect
