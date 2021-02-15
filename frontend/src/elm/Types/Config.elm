module Types.Config exposing (Config, init, updateFromServerSetup)

{-| Configuration values provided by the server
-}

import Types.Selection as Selection
import Types.ServerSetup exposing (ServerSetup)


{-| Configuration variables that are made available to most modules via their Context type
-}
type alias Config =
    { defaultPageSize : Int
    , defaultSorting : Selection.Sorting
    , numberOfFacetValues : Int
    }


{-| Initialize with standard values. These may get overwritten when we have fetched ServerSetup.
-}
init : Config
init =
    { defaultPageSize = 10
    , defaultSorting = Selection.ByRank
    , numberOfFacetValues = 20
    }


updateFromServerSetup : ServerSetup -> Config -> Config
updateFromServerSetup serverSetup config =
    { config
        | defaultPageSize =
            serverSetup.config.defaultPageSize |> Maybe.withDefault config.defaultPageSize
        , defaultSorting =
            serverSetup.config.defaultSorting |> Maybe.withDefault config.defaultSorting
        , numberOfFacetValues =
            serverSetup.config.numberOfFacetValues |> Maybe.withDefault config.numberOfFacetValues
    }
