module Types.ServerSetup exposing (Config, ServerSetup)

{-| Configuration values provided by the server
-}

import Types.Selection as Selection


type alias ServerSetup =
    { config : Config
    }


type alias Config =
    { defaultPageSize : Maybe Int
    , defaultSorting : Maybe Selection.Sorting
    , numberOfFacetValues : Maybe Int
    }
