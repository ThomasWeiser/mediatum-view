module Tests.Route exposing (fuzzerRoute)

import Data.Types exposing (FtsSorting(..), NodeId, nodeIdFromInt, nodeIdToInt)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Nonempty exposing (Nonempty)
import Route exposing (Route, RouteParameters, RoutePath(..))
import Set
import String.Extra
import TestUtils exposing (..)
import Tests.Data.Types exposing (..)


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
                (fuzzerYearRange
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


{-| A fuzzer for canonical search terms,
i.e. without whitespace at the left and the right
and without repeated whitespace within the string.

It generates some common search string formats as well as random search strings.

-}
fuzzerSearchTerm : Fuzzer String
fuzzerSearchTerm =
    Fuzz.oneOf
        [ [ "foo"
          , "foo bar"
          , "\"foo\""
          , "\"foo bar\""
          , "\"foo bar\" baz"
          , "-foo"
          , "foo -bar -baz"
          , "-\"foo bar\" baz"
          ]
            |> List.map Fuzz.constant
            |> Fuzz.oneOf
        , Fuzz.string
            |> Fuzz.map
                (String.Extra.clean >> String.Extra.nonBlank >> Maybe.withDefault "baz")
        ]


{-| A fuzzer for numbers representing years,
favoring years from 1960 to 2040,
but also generating random years between 0 and 99999.
-}
fuzzerYear : Fuzzer Int
fuzzerYear =
    Fuzz.frequency
        [ ( 1, Fuzz.intRange 0 99999 )
        , ( 5, Fuzz.intRange 1960 2040 )
        ]


{-| A fuzzer for pairs of years, favoring canonically sorted pairs.
-}
fuzzerYearRange : Fuzzer ( Int, Int )
fuzzerYearRange =
    Fuzz.andMap
        (Fuzz.tuple
            ( fuzzerYear, fuzzerYear )
        )
        (Fuzz.frequency
            [ ( 1, Fuzz.constant identity )
            , ( 3
              , Fuzz.constant
                    (\( year1, year2 ) ->
                        if year1 <= year2 then
                            ( year1, year2 )

                        else
                            ( year2, year1 )
                    )
              )
            ]
        )


fuzzerOffset : Fuzzer Int
fuzzerOffset =
    Fuzz.frequency
        [ ( 100, Fuzz.constant 0 )
        , ( 5, Fuzz.constant 5 )
        , ( 50, Fuzz.constant 10 )
        , ( 10, Fuzz.constant 20 )
        , ( 5, Fuzz.constant 50 )
        , ( 30, Fuzz.intRange 0 200 )
        ]


fuzzerLimit : Fuzzer Int
fuzzerLimit =
    Fuzz.frequency
        [ ( 10, Fuzz.constant 0 )
        , ( 5, Fuzz.constant 5 )
        , ( 50, Fuzz.constant 10 )
        , ( 10, Fuzz.constant 20 )
        , ( 5, Fuzz.constant 50 )
        , ( 30, Fuzz.intRange 0 200 )
        ]
