module Types.ServerSetup exposing (ServerConfig, ServerSetup)

{-| Configuration values provided by the server
-}

import Types.Selection as Selection


type alias ServerSetup =
    { config : ServerConfig
    }


type alias ServerConfig =
    { defaultPageSize : Maybe Int
    , defaultSorting : Maybe Selection.Sorting
    , numberOfFacetValues : Maybe Int
    }
