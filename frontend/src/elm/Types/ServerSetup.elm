module Types.ServerSetup exposing (Config, Server, ServerSetup)

{-| Configuration values provided by the server
-}

import Types.Selection as Selection


type alias ServerSetup =
    { server : Server
    , config : Config
    }


type alias Server =
    { apiVersion : Int
    , serverName : String
    }


type alias Config =
    { defaultPageSize : Maybe Int
    , defaultSorting : Maybe Selection.Sorting
    , numberOfFacetValues : Maybe Int
    }
