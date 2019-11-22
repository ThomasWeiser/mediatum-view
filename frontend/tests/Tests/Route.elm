module Tests.Route exposing (fuzzerRoute)

import Fuzz exposing (Fuzzer, int, list, string)
import List.Nonempty exposing (Nonempty)
import Route exposing (Route, RouteParameters, RoutePath(..))
import Set
import String.Extra
import TestUtils exposing (..)
import Tests.Types exposing (..)
import Tests.Types.Range
import Tests.Types.SearchTerm exposing (fuzzerSearchTerm)
import Types.SearchTerm
import Types.Selection exposing (FtsSorting(..))


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
                )
            |> Fuzz.andMap
                (Fuzz.oneOf [ Fuzz.constant FtsByRank, Fuzz.constant FtsByDate ])
            |> Fuzz.andMap
                (Tests.Types.Range.fuzzerRange fuzzerYear
                    |> Fuzz.maybe
                )
            |> Fuzz.andMap
                (TestUtils.shortList 4 fuzzerSearchTerm
                    |> Fuzz.map Types.SearchTerm.setFromList
                )
            |> Fuzz.andMap
                fuzzerOffset
            |> Fuzz.andMap
                fuzzerLimit
        )
