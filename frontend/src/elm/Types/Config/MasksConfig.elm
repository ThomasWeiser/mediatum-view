module Types.Config.MasksConfig exposing
    ( MasksConfig, MaskPurpose(..)
    , init
    , forPurpose
    )

{-| Configuration of the mediaTUM mask names used for various purposes.

Note that the mask name used is also dependant on the configured uiLanguage.

@docs MasksConfig, MaskPurpose
@docs init
@docs forPurpose

-}

import Types.Localization exposing (Translations)


{-| -}
type MaskPurpose
    = MaskForListing
    | MaskForDetails


{-| -}
type MasksConfig
    = MasksConfig
        { forListing : Translations
        , forDetails : Translations
        }


{-| Default values; may be overwritten by the server setup
-}
init : MasksConfig
init =
    MasksConfig
        { forListing =
            { en = "nodesmall_en"
            , de = "nodesmall"
            }
        , forDetails =
            { en = "nodebig_en"
            , de = "nodebig"
            }
        }


{-| Get the mask name for a given purpose. Returns all language translations.
-}
forPurpose : MaskPurpose -> MasksConfig -> Translations
forPurpose purpose (MasksConfig masks) =
    case purpose of
        MaskForListing ->
            masks.forListing

        MaskForDetails ->
            masks.forDetails
