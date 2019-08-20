module Tests.Route exposing (fuzzerRoute)

import Data.Types exposing (FtsSorting(..), NodeId, nodeIdFromInt, nodeIdToInt)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Nonempty exposing (Nonempty)
import Route exposing (Route, RouteParameters, RoutePath(..))
import Set
import String.Extra
import TestUtils exposing (..)
import Tests.Data.Types exposing (..)
import Tests.Range


fuzzerRoute : Fuzzer Route
fuzzerRoute =
    Fuzz.map2 Route
        (Fuzz.frequency
            [ ( 10, Fuzz.constant Route.NoId )
            , ( 20, Fuzz.map Route.OneId fuzzerNodeId )
            , ( 30, Fuzz.map2 Route.TwoIds fuzzerNodeId fuzzerNodeId )
            ]
        )
        (Fuzz.constant RouteParameters
            |> Fuzz.andMap
                (fuzzerSearchTerm
                    |> Fuzz.maybe
                    |> Fuzz.map (Maybe.withDefault "")
                )
            |> Fuzz.andMap
                (Fuzz.oneOf [ Fuzz.constant FtsByRank, Fuzz.constant FtsByDate ])
            |> Fuzz.andMap
                (Tests.Range.fuzzerRange fuzzerYear
                    |> Fuzz.maybe
                )
            |> Fuzz.andMap
                (TestUtils.shortList 4 fuzzerSearchTerm
                    |> Fuzz.map Set.fromList
                )
            |> Fuzz.andMap
                fuzzerOffset
            |> Fuzz.andMap
                fuzzerLimit
        )
