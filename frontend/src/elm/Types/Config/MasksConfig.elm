module Types.Config.MasksConfig exposing
    ( MasksConfig, MaskPurpose(..)
    , MasksPurposeServerConfig
    , init, updateFromServer
    , forPurpose
    )

{-| Configuration of the mediaTUM mask names used for various purposes and ui languages.

@docs MasksConfig, MaskPurpose
@docs MasksPurposeServerConfig
@docs init, updateFromServer
@docs forPurpose

-}

import Types.Localization exposing (Translations)


{-| -}
type MaskPurpose
    = MaskForListing
    | MaskForDetails


{-| Configuration of the mask names as used in the client

It's an opaque type. Use the provided functions to access the content.

-}
type MasksConfig
    = MasksConfig
        { forListing : Translations
        , forDetails : Translations
        }


{-| Configuration of the mask names as provided by the server
-}
type alias MasksPurposeServerConfig =
    { purpose : String
    , maskNames : Translations
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


{-| Adopt the configuration from the server
-}
updateFromServer : List MasksPurposeServerConfig -> MasksConfig -> MasksConfig
updateFromServer listOfMasksPurposeServerConfig masksConfig =
    listOfMasksPurposeServerConfig
        |> List.foldl
            (\{ purpose, maskNames } ->
                updateForPurpose purpose maskNames
            )
            masksConfig


{-| Set the mask names for a specific purpose
-}
updateForPurpose : String -> Translations -> MasksConfig -> MasksConfig
updateForPurpose purpose translations (MasksConfig masksConfig) =
    MasksConfig <|
        case purpose of
            "listing" ->
                { masksConfig | forListing = translations }

            "details" ->
                { masksConfig | forDetails = translations }

            _ ->
                masksConfig


{-| Get the mask name for a given purpose. Returns all language translations.
-}
forPurpose : MaskPurpose -> MasksConfig -> Translations
forPurpose purpose (MasksConfig masks) =
    case purpose of
        MaskForListing ->
            masks.forListing

        MaskForDetails ->
            masks.forDetails
