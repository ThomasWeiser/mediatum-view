module Types.AdjustmentToSetup exposing (AdjustmentToSetup(..))

{-|

@docs AdjustmentToSetup

-}

import Types.Localization exposing (Language)


{-| Adjustments to the setup that may be triggered by an UI component,
like switching the UI language.
-}
type AdjustmentToSetup
    = UserSelectedUILanguage Language
    | HideThumbnails Bool
