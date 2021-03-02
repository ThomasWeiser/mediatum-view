module Types.Config.MasksConfig exposing
    ( MaskPurpose
    , MasksConfig
    , forPurpose
    , init
    )

{-| ... TODO

@docs ... TODO

-}

import Types.Localization as Localization exposing (Language, Translations)


type MaskPurpose
    = MaskForListing
    | MaskForDetails


type MasksConfig
    = MasksConfig
        { forListing : Translations
        , forDetails : Translations
        }


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


forPurpose : MaskPurpose -> MasksConfig -> Translations
forPurpose purpose (MasksConfig masks) =
    case purpose of
        MaskForListing ->
            masks.forListing

        MaskForDetails ->
            masks.forDetails
