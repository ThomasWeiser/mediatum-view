module Types.ServerSetup exposing (ServerConfig, ServerSetup)

{-| Configuration values provided by the server
-}

import Types.Config.FacetAspect exposing (FacetAspect)
import Types.Config.FtsAspect exposing (FtsAspect)
import Types.Selection as Selection


type alias ServerSetup =
    { config : ServerConfig
    }


type alias ServerConfig =
    { defaultPageSize : Maybe Int
    , defaultSorting : Maybe Selection.Sorting
    , numberOfFacetValues : Maybe Int
    , staticFtsAspects : Maybe (List FtsAspect)
    , staticFacetAspects : Maybe (List FacetAspect)
    }
