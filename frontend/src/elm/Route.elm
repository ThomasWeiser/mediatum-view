module Route exposing
    ( Route
    , RouteParameters
    , RoutePath(..)
    , cleanSearchTerm
    , fromOneId
    , home
    )

import Data.Types exposing (FtsSorting(..), NodeId)
import List.Nonempty exposing (Nonempty)
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
    { ftsTerm : Maybe String
    , ftsSorting : Maybe FtsSorting
    , filterByYear : Maybe ( Int, Int )
    , filterByTitle : Maybe (Nonempty String)
    , offset : Maybe Int
    , limit : Maybe Int
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
    { ftsTerm = Nothing
    , ftsSorting = Nothing
    , filterByYear = Nothing
    , filterByTitle = Nothing
    , offset = Nothing
    , limit = Nothing
    }


cleanSearchTerm : String -> Maybe String
cleanSearchTerm =
    -- Trim the whitespace of both sides of the string
    -- and compress repeated whitespace internally to a single whitespace char.
    String.Extra.clean >> String.Extra.nonBlank
