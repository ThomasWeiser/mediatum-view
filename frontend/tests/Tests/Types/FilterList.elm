module Tests.Types.FilterList exposing (fuzzerAspectName, fuzzerFilterList, suite)

import Expect
import Fuzz exposing (Fuzzer)
import Ordering
import Test exposing (..)
import TestUtils exposing (..)
import Types.Aspect as Aspect exposing (Aspect)
import Types.FilterList as FilterList exposing (FilterList)


suite : Test
suite =
    describe "Types.FilterList"
        [ test "fromList has same result as multiple inserts" <|
            \() ->
                Expect.equal
                    filterListA1B2C3fromList
                    filterListA1B2C3
        , test "toList" <|
            \() ->
                Expect.equal
                    (FilterList.toList filterListA1B2C3)
                    [ ( Aspect.fromString "A", 1 )
                    , ( Aspect.fromString "B", 2 )
                    , ( Aspect.fromString "C", 3 )
                    ]
        , fuzz (fuzzerFilterList (Fuzz.intRange 0 3)) "toList->fromList" <|
            \filterList ->
                Expect.equal
                    (filterList |> FilterList.toList |> FilterList.fromList)
                    filterList
        , describe "update"
            [ test "empty list" <|
                \() ->
                    Expect.equal
                        (FilterList.init
                            |> FilterList.update (Aspect.fromString "N") (always <| Just 4)
                            |> FilterList.toList
                        )
                        [ ( Aspect.fromString "N", 4 ) ]
            , test "non-existing aspect, nothing" <|
                \() ->
                    Expect.equal
                        (filterListA1B2C3
                            |> FilterList.update (Aspect.fromString "N") (always Nothing)
                        )
                        filterListA1B2C3
            , test "non-existing aspect, just constant" <|
                \() ->
                    Expect.equal
                        (filterListA1B2C3
                            |> FilterList.update (Aspect.fromString "N") (always <| Just 4)
                        )
                        filterListA1B2C3N4
            , test "existing aspect, remove" <|
                \() ->
                    Expect.equal
                        (filterListA1B2C3
                            |> FilterList.update (Aspect.fromString "B") (always Nothing)
                            |> FilterList.toList
                        )
                        [ ( Aspect.fromString "A", 1 )
                        , ( Aspect.fromString "C", 3 )
                        ]
            , test "existing aspect, map" <|
                \() ->
                    Expect.equal
                        (filterListA1B2C3
                            |> FilterList.update (Aspect.fromString "B") (Maybe.map ((+) 300))
                            |> FilterList.toList
                        )
                        [ ( Aspect.fromString "A", 1 )
                        , ( Aspect.fromString "B", 302 )
                        , ( Aspect.fromString "C", 3 )
                        ]
            ]
        , testCoarseOrderingProperties
            "FilterList"
            (fuzzerFilterList (Fuzz.intRange 0 3))
            (FilterList.ordering Ordering.natural)
        , test "ordering is independent of insert-order" <|
            \() ->
                Expect.equal
                    (FilterList.ordering Ordering.natural filterListA1B2C3 filterListC3B2A1)
                    EQ
        ]


filterListA1B2C3fromList : FilterList Int
filterListA1B2C3fromList =
    [ ( Aspect.fromString "A", 101 )
    , ( Aspect.fromString "B", 102 )
    , ( Aspect.fromString "A", 1 )
    , ( Aspect.fromString "B", 2 )
    , ( Aspect.fromString "C", 3 )
    ]
        |> FilterList.fromList


filterListA1B2C3 : FilterList Int
filterListA1B2C3 =
    FilterList.init
        |> FilterList.insert (Aspect.fromString "A") 1
        |> FilterList.insert (Aspect.fromString "B") 202
        |> FilterList.insert (Aspect.fromString "C") 203
        |> FilterList.insert (Aspect.fromString "B") 2
        |> FilterList.insert (Aspect.fromString "C") 3


filterListA1B2C3N4 : FilterList Int
filterListA1B2C3N4 =
    filterListA1B2C3
        |> FilterList.insert (Aspect.fromString "N") 4


filterListC3B2A1 : FilterList Int
filterListC3B2A1 =
    FilterList.init
        |> FilterList.insert (Aspect.fromString "C") 3
        |> FilterList.insert (Aspect.fromString "B") 2
        |> FilterList.insert (Aspect.fromString "A") 1


fuzzerFilterList : Fuzzer v -> Fuzzer (FilterList v)
fuzzerFilterList fuzzerValue =
    Fuzz.map2 Tuple.pair
        fuzzerAspectName
        fuzzerValue
        |> shortListUniqueBy (Tuple.first >> Aspect.toString) 4
        |> Fuzz.map FilterList.fromList


fuzzerAspectName : Fuzzer Aspect
fuzzerAspectName =
    [ "title", "author", "person", "subject" ]
        |> List.map Aspect.fromString
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
