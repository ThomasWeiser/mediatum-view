module Types.Config exposing
    ( Config
    , init
    , updateFromServerSetup
    , adjustUILanguage
    , getMaskName
    )

{-| Configuration values used throughout the app.

Most values are defined in the server and fetched dynamically.

@docs Config
@docs init
@docs updateFromServerSetup
@docs adjustUILanguage
@docs getMaskName

-}

import Maybe exposing (Maybe)
import Maybe.Extra
import Types.Config.FacetAspectConfig exposing (FacetAspectConfig)
import Types.Config.FtsAspectConfig exposing (FtsAspectConfig)
import Types.Config.MasksConfig as MasksConfig exposing (MasksConfig)
import Types.Localization as Localization exposing (Language)
import Types.Selection as Selection
import Types.ServerSetup exposing (ServerSetup)


{-| Configuration values that are made available to most modules via their Context type
-}
type alias Config =
    { uiLanguage : Language
    , serverConfigAdopted : Bool
    , defaultPageSize : Int
    , defaultSorting : Selection.Sorting
    , numberOfFacetValues : Int
    , ftsAspects : List FtsAspectConfig
    , facetAspects : List FacetAspectConfig
    , masks : MasksConfig
    }


{-| Initialize with standard values.
-}
init : Config
init =
    { uiLanguage = Localization.LangEn
    , serverConfigAdopted = False
    , defaultPageSize = 10
    , defaultSorting = Selection.ByRank
    , numberOfFacetValues = 20
    , ftsAspects = []
    , facetAspects = []
    , masks = MasksConfig.init
    }


{-| Set the `uiLanguage` from given `navigatorLanguage` and `userSelectedUILanguage`
-}
adjustUILanguage : Maybe Language -> Maybe Language -> Config -> Config
adjustUILanguage navigatorLanguage userSelectedUILanguage config =
    { config
        | uiLanguage =
            [ userSelectedUILanguage
            , navigatorLanguage
            ]
                |> Maybe.Extra.orList
                |> Maybe.withDefault Localization.LangEn
    }


{-| -}
updateFromServerSetup : ServerSetup -> Config -> Config
updateFromServerSetup serverSetup config =
    { config
        | serverConfigAdopted = True
        , defaultPageSize =
            serverSetup.config.defaultPageSize |> Maybe.withDefault config.defaultPageSize
        , defaultSorting =
            serverSetup.config.defaultSorting |> Maybe.withDefault config.defaultSorting
        , numberOfFacetValues =
            serverSetup.config.numberOfFacetValues |> Maybe.withDefault config.numberOfFacetValues
        , ftsAspects =
            serverSetup.config.staticFtsAspects |> Maybe.withDefault []
        , facetAspects =
            serverSetup.config.staticFacetAspects |> Maybe.withDefault []
    }


{-| Get the name of a mask as configured for the current uiLanguage and the given purpose.
-}
getMaskName : MasksConfig.MaskPurpose -> Config -> String
getMaskName purpose config =
    MasksConfig.forPurpose purpose config.masks
        |> Localization.string config
