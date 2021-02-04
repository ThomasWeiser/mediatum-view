module Tests.Types.FtsFilterLine exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import String exposing (String)
import Test exposing (..)
import TestUtils exposing (..)
import Types.Aspect as Aspect
import Types.FtsFilterLine exposing (..)
import Utils


aspect =
    { a = Aspect.fromString "A"
    , b = Aspect.fromString "B"
    , c = Aspect.fromString "C"
    , d = Aspect.fromString "D"
    , e = Aspect.fromString "E"
    , f = Aspect.fromString "F"
    , z = Aspect.fromString "Z"
    }


uiModel1 : FtsFilterLines
uiModel1 =
    [ ( aspect.z, "" )
    , ( aspect.a, "A" )
    , ( aspect.b, "" )
    , ( aspect.c, "C" )
    , ( aspect.d, "" )
    , ( aspect.e, "E" )
    ]


suite : Test
suite =
    describe "Types.FtsFilterLine"
        [ describe "special cases"
            [ test "Route (a c)" <|
                expectResultFromUiModel1AndGivenRoute
                    [ ( aspect.a, "An" )
                    , ( aspect.c, "Cn" )
                    ]
                    [ ( aspect.z, "" )
                    , ( aspect.a, "An" )
                    , ( aspect.b, "" )
                    , ( aspect.c, "Cn" )
                    , ( aspect.d, "" )
                    ]
            , test "Route (c a)" <|
                expectResultFromUiModel1AndGivenRoute
                    [ ( aspect.c, "Cn" )
                    , ( aspect.a, "An" )
                    ]
                    [ ( aspect.z, "" )
                    , ( aspect.c, "Cn" )
                    , ( aspect.a, "An" )
                    , ( aspect.b, "" )
                    , ( aspect.d, "" )
                    ]
            , test "Route (b)" <|
                expectResultFromUiModel1AndGivenRoute
                    [ ( aspect.b, "Bn" )
                    ]
                    [ ( aspect.z, "" )
                    , ( aspect.b, "Bn" )
                    , ( aspect.d, "" )
                    ]
            , test "Route (b d)" <|
                expectResultFromUiModel1AndGivenRoute
                    [ ( aspect.b, "Bn" )
                    , ( aspect.d, "Dn" )
                    ]
                    [ ( aspect.z, "" )
                    , ( aspect.b, "Bn" )
                    , ( aspect.d, "Dn" )
                    ]
            , test "Route (d b)" <|
                expectResultFromUiModel1AndGivenRoute
                    [ ( aspect.d, "Dn" )
                    , ( aspect.b, "Bn" )
                    ]
                    [ ( aspect.z, "" )
                    , ( aspect.d, "Dn" )
                    , ( aspect.b, "Bn" )
                    ]
            , test "Route (f)" <|
                expectResultFromUiModel1AndGivenRoute
                    [ ( aspect.f, "Fn" )
                    ]
                    [ ( aspect.f, "Fn" )
                    , ( aspect.z, "" )
                    , ( aspect.b, "" )
                    , ( aspect.d, "" )
                    ]
            , test "Route (c f)" <|
                expectResultFromUiModel1AndGivenRoute
                    [ ( aspect.c, "Cn" )
                    , ( aspect.f, "Fn" )
                    ]
                    [ ( aspect.z, "" )
                    , ( aspect.b, "" )
                    , ( aspect.c, "Cn" )
                    , ( aspect.f, "Fn" )
                    , ( aspect.d, "" )
                    ]
            ]
        , describe "property based test"
            [ fuzzTest "fromRoute should be a sub-sequence of new uiModel" <|
                \fromRoute uiModel ->
                    mergeFtsFilterLines fromRoute uiModel
                        |> List.Extra.isSubsequenceOf fromRoute
                        |> Expect.true "not a sub-sequence"
            , fuzzTest "emtpy line from uiModel that are not overwritten in fromRoute should be a sub-sequence of new uiModel" <|
                \fromRoute uiModel ->
                    mergeFtsFilterLines fromRoute uiModel
                        |> List.Extra.isSubsequenceOf
                            (uiModel |> List.filter (isEmptyAndNotInList fromRoute))
                        |> Expect.true "not a sub-sequence"
            , fuzzTest "new uiModel should have as many lines as there are relevant input lines" <|
                \fromRoute uiModel ->
                    mergeFtsFilterLines fromRoute uiModel
                        |> List.length
                        |> Expect.equal
                            (List.length fromRoute
                                + List.length (uiModel |> List.filter (isEmptyAndNotInList fromRoute))
                            )
            , fuzzTest "lines new uiModel should not have duplicated aspects" <|
                \fromRoute uiModel ->
                    mergeFtsFilterLines fromRoute uiModel
                        |> List.Extra.allDifferentBy (Tuple.first >> Aspect.toString)
                        |> Expect.true "newUiModel has more than one line for the same aspect"
            , fuzzTest "emtpy lines from uiModel should not occur early in new uiModel" <|
                \fromRoute uiModel ->
                    let
                        newUiModel =
                            -- List.reverse <| -- For testing the test: provoke failures
                            mergeFtsFilterLines fromRoute uiModel
                    in
                    expectationList
                        (List.Extra.lift2
                            (\( ( m1aspect, _ ), ( m2aspect, _ ) ) ( ( n1aspect, n1value ), ( n2aspect, _ ) ) ->
                                if
                                    String.isEmpty n1value
                                        && (m1aspect == n2aspect)
                                        && (m2aspect == n1aspect)
                                then
                                    Expect.fail <|
                                        "Empty line "
                                            ++ Aspect.toString m2aspect
                                            ++ " found before line "
                                            ++ Aspect.toString m1aspect
                                            ++ " in newUiModel ["
                                            ++ String.join ","
                                                (List.map (Tuple.first >> Aspect.toString) newUiModel)
                                            ++ "]"

                                else
                                    Expect.pass
                            )
                            (List.Extra.uniquePairs uiModel)
                            (List.Extra.uniquePairs newUiModel)
                        )
            ]
        ]


expectResultFromUiModel1AndGivenRoute : FtsFilterLines -> FtsFilterLines -> () -> Expect.Expectation
expectResultFromUiModel1AndGivenRoute fromRoute expectedNewUiModel =
    \() ->
        mergeFtsFilterLines fromRoute
            uiModel1
            |> Expect.equal expectedNewUiModel


fuzzTest : String -> (FtsFilterLines -> FtsFilterLines -> Expect.Expectation) -> Test
fuzzTest =
    fuzz2
        fuzzerFilterLinesFromRoute
        fuzzerFilterLinesUiModel


fuzzerFilterLinesFromRoute : Fuzzer FtsFilterLines
fuzzerFilterLinesFromRoute =
    fuzzerAspectName
        |> Fuzz.map
            (\x ->
                ( Aspect.fromString x
                , x ++ "n"
                )
            )
        |> shortListUniqueBy (Tuple.first >> Aspect.toString) 6
        |> Fuzz.map (Debug.log "fuzzerFilterLinesFromRoute")


fuzzerFilterLinesUiModel : Fuzzer FtsFilterLines
fuzzerFilterLinesUiModel =
    Fuzz.map2
        (\x empty ->
            ( Aspect.fromString x
            , empty |> Utils.ifElse "" x
            )
        )
        fuzzerAspectName
        Fuzz.bool
        |> shortListUniqueBy (Tuple.first >> Aspect.toString) 7


fuzzerAspectName : Fuzzer String
fuzzerAspectName =
    [ "A", "B", "C", "D", "E", "F" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
