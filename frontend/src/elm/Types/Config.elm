module Types.Config exposing
    ( Config
    , init, initFromFlags
    , updateFromJSConfigChangeEvent, updateFromServerSetup
    , setUiLanguage
    )

{-| Configuration values used throughout the app.

Most values are defined in the server and fetched dynamically.

@docs Config
@docs init, initFromFlags
@docs updateFromJSConfigChangeEvent, updateFromServerSetup
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
    { navigatorLanguage : Maybe Language
    , userSelectedUILanguage : Maybe Language
    , uiLanguage : Language
    , serverConfigAdopted : Bool
    , defaultPageSize : Int
    , defaultSorting : Selection.Sorting
    , numberOfFacetValues : Int
    , ftsAspects : List FtsAspectConfig
    , facetAspects : List FacetAspectConfig
    }


{-| Initialize with standard values.
-}
init : Config
init =
    { navigatorLanguage = Nothing
    , userSelectedUILanguage = Nothing
    , uiLanguage = Localization.LangEn
    , serverConfigAdopted = False
    , defaultPageSize = 10
    , defaultSorting = Selection.ByRank
    , numberOfFacetValues = 20
    , ftsAspects = []
    , facetAspects = []
    }


{-| Parameters passed from JS on startup
-}
type alias Flags =
    { navigatorLanguage : Maybe Language
    , storedSelectedUILanguage : Maybe Language
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


{-| Change-events passed from JS
-}
type alias JSConfigChangeEvent =
    { changedNavigatorLanguage : Maybe Language
    , changedSelectedUILanguage : Maybe Language
    }


decoderConfigChangeEvent : JD.Decoder JSConfigChangeEvent
decoderConfigChangeEvent =
    JD.map2 JSConfigChangeEvent
        (JD.field "changedNavigatorLanguageTag" JD.string
            |> JD.maybe
            |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
        )
        (JD.field "changedSelectedUILanguageTag" JD.string
            |> JD.maybe
            |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
        )


{-| -}
initFromFlags : JD.Value -> Config
initFromFlags flagsJsonValue =
    case JD.decodeValue decoderFlags flagsJsonValue of
        Err err ->
            init

        Ok flags ->
            { init
                | navigatorLanguage = flags.navigatorLanguage
                , userSelectedUILanguage = flags.storedSelectedUILanguage
            }
                |> adjustUILanguage


updateFromJSConfigChangeEvent : JD.Value -> Config -> Config
updateFromJSConfigChangeEvent eventJsonValue config =
    case JD.decodeValue decoderConfigChangeEvent eventJsonValue of
        Err err ->
            config

        Ok event ->
            let
                _ =
                    Debug.log "updateFromJSConfigChangeEvent" event
            in
            { config
                | navigatorLanguage =
                    Maybe.Extra.or
                        event.changedNavigatorLanguage
                        config.navigatorLanguage
                , userSelectedUILanguage =
                    Maybe.Extra.or
                        event.changedSelectedUILanguage
                        config.userSelectedUILanguage
            }
                |> adjustUILanguage


{-| -}
setUiLanguage : Language -> Config -> Config
setUiLanguage language config =
    { config
        | userSelectedUILanguage = Just language
    }
        |> adjustUILanguage


adjustUILanguage : Config -> Config
adjustUILanguage config =
    { config
        | uiLanguage =
            [ config.userSelectedUILanguage
            , config.navigatorLanguage
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
