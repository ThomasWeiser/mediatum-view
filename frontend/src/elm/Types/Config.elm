module Types.Config exposing
    ( Config
    , init, updateFromFlags, updateFromJSConfigChangeEvent, updateFromServerSetup
    , setUiLanguage
    )

{-| Configuration values used throughout the app.

Most values are defined in the server and fetched dynamically.

@docs Config
@docs init, updateFromFlags, updateFromJSConfigChangeEvent, updateFromServerSetup
@docs setUiLanguage

-}

import Json.Decode as JD
import Maybe exposing (Maybe)
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
    , userSelectedUILanguageTag : Maybe Language
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
    , userSelectedUILanguageTag = Nothing
    , uiLanguage = Localization.LangEn
    , serverConfigAdopted = False
    , defaultPageSize = 10
    , defaultSorting = Selection.ByRank
    , numberOfFacetValues = 20
    , ftsAspects = []
    , facetAspects = []
    }


type alias Flags =
    { navigatorLanguageTag : Maybe Language
    , userSelectedUILanguageTag : Maybe Language
    }


decoderFlags : JD.Decoder Flags
decoderFlags =
    JD.map2 Flags
        (JD.field "navigatorLanguageTag" JD.string
            |> JD.maybe
            |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
        )
        (JD.field "userSelectedUILanguageTag" JD.string
            |> JD.maybe
            |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
        )


initFlags : Flags
initFlags =
    { navigatorLanguageTag = Nothing
    , userSelectedUILanguageTag = Nothing
    }


type alias ConfigChangeEvent =
    { navigatorLanguageTag : Maybe Language
    , userSelectedUILanguageTag : Maybe Language
    }


decoderConfigChangeEvent : JD.Decoder ConfigChangeEvent
decoderConfigChangeEvent =
    JD.map2 ConfigChangeEvent
        (JD.field "navigatorLanguageTag" JD.string
            |> JD.maybe
            |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
        )
        (JD.field "userSelectedUILanguageTag" JD.string
            |> JD.maybe
            |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
        )


adjustUILanguage : Config -> Config
adjustUILanguage config =
    { config
        | uiLanguage =
            [ config.userSelectedUILanguageTag
            , config.flags.userSelectedUILanguageTag
            , config.flags.navigatorLanguageTag
            ]
                |> Maybe.Extra.orList
                |> Maybe.withDefault Localization.LangEn
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

                {- , uiLanguage =
                   [ flags.userSelectedUILanguageTag
                   , flags.navigatorLanguageTag
                   ]
                       |> List.map (Maybe.andThen Localization.languageFromLanguageTag)
                       |> Maybe.Extra.orList
                       |> Maybe.withDefault config.uiLanguage
                -}
            }
                |> adjustUILanguage


updateFromJSConfigChangeEvent : JD.Value -> Config -> Config
updateFromJSConfigChangeEvent eventJsonValue config =
    case JD.decodeValue decoderConfigChangeEvent eventJsonValue of
        Err err ->
            config

        Ok event ->
            { config
                | userSelectedUILanguageTag =
                    Maybe.Extra.or
                        event.userSelectedUILanguageTag
                        config.userSelectedUILanguageTag
            }
                |> adjustUILanguage


{-| -}
setUiLanguage : Language -> Config -> Config
setUiLanguage language config =
    { config
        | userSelectedUILanguageTag = Just language
    }
        |> adjustUILanguage


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
