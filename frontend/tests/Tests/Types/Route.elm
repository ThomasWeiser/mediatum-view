module Tests.Types.Route exposing (fuzzerRoute)

import Fuzz exposing (Fuzzer)
import Tests.Types exposing (..)
import Types.Route as Route exposing (Route, RouteParameters)
import Types.Selection exposing (Sorting(..))


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
                fuzzerGlobalSearch
            |> Fuzz.andMap
                fuzzerSorting
            |> Fuzz.andMap
                Tests.Types.fuzzerFtsFilters
            |> Fuzz.andMap
                Tests.Types.fuzzerFacetFilters
            |> Fuzz.andMap
                fuzzerOffset
            |> Fuzz.andMap
                fuzzerLimit
        )
