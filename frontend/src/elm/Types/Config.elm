module Types.Config exposing
    ( Config
    , init
    , updateFromServerSetup
    )

{-| Configuration values provided by the server
-}

import Types.Config.FacetAspectConfig exposing (FacetAspectConfig)
import Types.Config.FtsAspectConfig exposing (FtsAspectConfig)
import Types.Selection as Selection
import Types.ServerSetup exposing (ServerSetup)


{-| Configuration variables that are made available to most modules via their Context type
-}
type alias Config =
    { serverConfigAdopted : Bool
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
    { serverConfigAdopted = False
    , defaultPageSize = 10
    , defaultSorting = Selection.ByRank
    , numberOfFacetValues = 20
    , ftsAspects = []
    , facetAspects = []
    }


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
