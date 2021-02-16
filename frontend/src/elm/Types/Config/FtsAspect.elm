module Types.Config.FtsAspect exposing (FtsAspect)

{-| TODO
-}

import Types.Aspect exposing (Aspect)
import Types.Localization as Localization


type alias FtsAspect =
    { aspect : Aspect
    , label : Localization.Translations
    }
