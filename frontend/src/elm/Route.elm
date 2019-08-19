module Route exposing
    ( Route
    , RouteParameters
    , RoutePath(..)
    , cleanSearchTerm
    , fromOneId
    , home
    )

import Data.Types exposing (FtsSorting(..), NodeId)
import String.Extra


type alias Route =
    { path : RoutePath
    , parameters : RouteParameters
    }


type RoutePath
    = NoId
    | OneId NodeId
    | TwoIds NodeId NodeId


type alias RouteParameters =
    { ftsTerm : String
    , ftsSorting : FtsSorting
    , filterByYear : Maybe ( Int, Int )
    , filterByTitle : List String
    , offset : Int
    , limit : Int
    }


fromOneId : NodeId -> Route
fromOneId nodeId =
    -- TODO: Only for adopting legacy code. To be removed later.
    { path = OneId nodeId
    , parameters = emptyParameters
    }


home : Route
home =
    { path = NoId
    , parameters = emptyParameters
    }


emptyParameters : RouteParameters
emptyParameters =
    { ftsTerm = ""
    , ftsSorting = FtsByRank
    , filterByYear = Nothing
    , filterByTitle = []
    , offset = 0
    , limit = 10
    }


cleanSearchTerm : String -> String
cleanSearchTerm =
    -- Trim the whitespace of both sides of the string
    -- and compress repeated whitespace internally to a single whitespace char.
    String.Extra.clean
