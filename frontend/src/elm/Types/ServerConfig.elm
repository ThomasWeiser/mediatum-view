module Types.ServerConfig exposing (Defaults, ServerConfig, init)

{-| Configuration values provided by the server
-}

import Types.Selection as Selection


type alias ServerConfig =
    { defaults : Defaults
    }


type alias Defaults =
    { pageSize : Int

    --, facetValuesToQuery : Int
    , sortBy : Selection.Sorting
    }


init : ServerConfig
init =
    { defaults =
        { pageSize = 10
        , sortBy = Selection.ByRank
        }
    }
