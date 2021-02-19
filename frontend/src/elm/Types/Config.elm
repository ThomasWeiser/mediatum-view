module Types.Config exposing
    ( Config
    , init, updateFromFlags, updateFromServerSetup
    , setUiLanguage
    )

{-| Configuration values used throughout the app.

Most values are defined in the server and fetched dynamically.

@docs Config
@docs init, updateFromFlags, updateFromServerSetup
@docs setUiLanguage

-}

import Json.Decode as JD
import Maybe.Extra
import Types.Config.FacetAspectConfig exposing (FacetAspectConfig)
import Types.Config.FtsAspectConfig exposing (FtsAspectConfig)
import Types.Localization as Localization exposing (Language)
import Types.Selection as Selection
import Types.ServerSetup exposing (ServerSetup)


{-| Configuration values that are made available to most modules via their Context type
-}
type alias Config =
    { flags : Flags
    , uiLanguage : Language
    , serverConfigAdopted : Bool
    , defaultPageSize : Int
    , defaultSorting : Selection.Sorting
    , numberOfFacetValues : Int
    , ftsAspects : List FtsAspectConfig
    , facetAspects : List FacetAspectConfig
    }


{-| Initialize with standard values. These may get overwritten when we have fetched ServerSetup.
-}
init : Config
init =
    { flags = initFlags
    , uiLanguage = Localization.LangEn
    , serverConfigAdopted = False
    , defaultPageSize = 10
    , defaultSorting = Selection.ByRank
    , numberOfFacetValues = 20
    , ftsAspects = []
    , facetAspects = []
    }


type alias Flags =
    { navigatorLanguageTag : Maybe String
    , userSelectedUILanguageTag : Maybe String
    }


initFlags : Flags
initFlags =
    { navigatorLanguageTag = Nothing
    , userSelectedUILanguageTag = Nothing
    }


{-| -}
updateFromFlags : JD.Value -> Config -> Config
updateFromFlags flagsJsonValue config =
    case JD.decodeValue decoderFlags flagsJsonValue of
        Err err ->
            config

        Ok flags ->
            { config
                | flags = flags
                , uiLanguage =
                    [ flags.userSelectedUILanguageTag
                    , flags.navigatorLanguageTag
                    ]
                        |> List.map (Maybe.andThen Localization.languageFromLanguageTag)
                        |> Maybe.Extra.orList
                        |> Maybe.withDefault config.uiLanguage
            }


decoderFlags : JD.Decoder Flags
decoderFlags =
    JD.map2 Flags
        (JD.maybe <| JD.field "navigatorLanguageTag" JD.string)
        (JD.maybe <| JD.field "userSelectedUILanguageTag" JD.string)


{-| -}
setUiLanguage : Language -> Config -> Config
setUiLanguage language config =
    { config
        | uiLanguage = language
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
