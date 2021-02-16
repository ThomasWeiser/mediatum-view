module Types.Config.FacetAspect exposing (FacetAspect)

{-| TODO
-}

import Types.Aspect exposing (Aspect)
import Types.Localization as Localization


type alias FacetAspect =
    { aspect : Aspect
    , label : Localization.Translations
    }
