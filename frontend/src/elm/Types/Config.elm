module Types.Config exposing
    ( Config
    , init
    , updateFromServerSetup
    , adjustUILanguage, adjustHideThumbnails
    , getMaskName
    )

{-| Configuration values used throughout the app.

Most values are defined in the server and fetched dynamically.

@docs Config
@docs init
@docs updateFromServerSetup
@docs adjustUILanguage, adjustHideThumbnails
@docs getMaskName

-}

import Maybe exposing (Maybe)
import Maybe.Extra
import Types.Config.FacetAspectConfig exposing (FacetAspectConfig)
import Types.Config.FtsAspectConfig exposing (FtsAspectConfig)
import Types.Config.MasksConfig as MasksConfig exposing (MasksConfig)
import Types.Id exposing (FolderId)
import Types.Localization as Localization exposing (Language, Translations)
import Types.Selection as Selection
import Types.ServerSetup exposing (ServerSetup)


{-| Configuration values that are made available to most modules via their Context type
-}
type alias Config =
    { uiLanguage : Language
    , serverConfigAdopted : Bool
    , toplevelFolderIds : List FolderId
    , defaultLimit : Int
    , maxLimit : Int
    , defaultSorting : Selection.Sorting
    , numberOfFacetValues : Int
    , numberOfFacetValuesShortList : Int
    , ftsAspects : List FtsAspectConfig
    , facetAspects : List FacetAspectConfig
    , masks : MasksConfig
    , hideThumbnails : Bool
    , frontPage : Maybe Translations
    }


{-| Initialize with standard values.
-}
init : Config
init =
    { uiLanguage = Localization.LangEn
    , serverConfigAdopted = False
    , toplevelFolderIds = []
    , defaultLimit = 10
    , maxLimit = 1000
    , defaultSorting = Selection.ByRank
    , numberOfFacetValues = 20
    , numberOfFacetValuesShortList = 5
    , ftsAspects = []
    , facetAspects = []
    , masks = MasksConfig.init
    , hideThumbnails = False
    , frontPage = Nothing
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


{-| Set the globally configured option to hide thumbnails in listing and details views
-}
adjustHideThumbnails : Bool -> Config -> Config
adjustHideThumbnails newState config =
    { config
        | hideThumbnails = newState
    }


{-| -}
updateFromServerSetup : ServerSetup -> Config -> Config
updateFromServerSetup serverSetup config =
    { config
        | serverConfigAdopted = True
        , toplevelFolderIds =
            serverSetup.config.toplevelFolderIds |> Maybe.withDefault config.toplevelFolderIds
        , defaultLimit =
            serverSetup.config.defaultLimit |> Maybe.withDefault config.defaultLimit
        , maxLimit =
            serverSetup.config.maxLimit |> Maybe.withDefault config.maxLimit
        , defaultSorting =
            serverSetup.config.defaultSorting |> Maybe.withDefault config.defaultSorting
        , numberOfFacetValues =
            serverSetup.config.numberOfFacetValues |> Maybe.withDefault config.numberOfFacetValues
        , ftsAspects =
            serverSetup.config.staticFtsAspects |> Maybe.withDefault config.ftsAspects
        , facetAspects =
            serverSetup.config.staticFacetAspects |> Maybe.withDefault config.facetAspects
        , masks =
            case serverSetup.config.masksByPurpose of
                Nothing ->
                    config.masks

                Just masksPurposeServerConfig ->
                    MasksConfig.updateFromServer masksPurposeServerConfig config.masks
        , frontPage = serverSetup.config.frontPage
    }


{-| Get the name of a mask as configured for the current uiLanguage and the given purpose.
-}
getMaskName : MasksConfig.MaskPurpose -> Config -> String
getMaskName purpose config =
    MasksConfig.forPurpose purpose config.masks
        |> Localization.string config
