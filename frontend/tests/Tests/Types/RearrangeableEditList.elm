module Tests.Types.RearrangeableEditList exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import String exposing (String)
import Test exposing (..)
import TestUtils exposing (..)
import Types.RearrangeableEditList exposing (..)
import Utils


type alias TestItem =
    RearrangeableEditItem String String


type alias TestList =
    RearrangeableEditList String String


testModel : TestList
testModel =
    [ ( "z", "" )
    , ( "a", "A" )
    , ( "b", "" )
    , ( "c", "C" )
    , ( "d", "" )
    , ( "e", "E" )
    ]


keep : TestItem -> Bool
keep =
    Tuple.second >> String.isEmpty


suite : Test
suite =
    describe "Types.RearrangeableEditList"
        [ describe "special cases"
            [ test "Route (a c)" <|
                expectResultFromTestModelAndGivenRoute
                    [ ( "a", "An" )
                    , ( "c", "Cn" )
                    ]
                    [ ( "z", "" )
                    , ( "a", "An" )
                    , ( "b", "" )
                    , ( "c", "Cn" )
                    , ( "d", "" )
                    ]
            , test "Route (c a)" <|
                expectResultFromTestModelAndGivenRoute
                    [ ( "c", "Cn" )
                    , ( "a", "An" )
                    ]
                    [ ( "z", "" )
                    , ( "c", "Cn" )
                    , ( "a", "An" )
                    , ( "b", "" )
                    , ( "d", "" )
                    ]
            , test "Route (b)" <|
                expectResultFromTestModelAndGivenRoute
                    [ ( "b", "Bn" )
                    ]
                    [ ( "z", "" )
                    , ( "b", "Bn" )
                    , ( "d", "" )
                    ]
            , test "Route (b d)" <|
                expectResultFromTestModelAndGivenRoute
                    [ ( "b", "Bn" )
                    , ( "d", "Dn" )
                    ]
                    [ ( "z", "" )
                    , ( "b", "Bn" )
                    , ( "d", "Dn" )
                    ]
            , test "Route (d b)" <|
                expectResultFromTestModelAndGivenRoute
                    [ ( "d", "Dn" )
                    , ( "b", "Bn" )
                    ]
                    [ ( "z", "" )
                    , ( "d", "Dn" )
                    , ( "b", "Bn" )
                    ]
            , test "Route (f)" <|
                expectResultFromTestModelAndGivenRoute
                    [ ( "f", "Fn" )
                    ]
                    [ ( "f", "Fn" )
                    , ( "z", "" )
                    , ( "b", "" )
                    , ( "d", "" )
                    ]
            , test "Route (c f)" <|
                expectResultFromTestModelAndGivenRoute
                    [ ( "c", "Cn" )
                    , ( "f", "Fn" )
                    ]
                    [ ( "z", "" )
                    , ( "b", "" )
                    , ( "c", "Cn" )
                    , ( "f", "Fn" )
                    , ( "d", "" )
                    ]
            ]
        , describe "property based test"
            [ fuzzTest "routeList should be a sub-sequence of rearranged editList" <|
                \routeList editList ->
                    rearrange keep routeList editList
                        |> List.Extra.isSubsequenceOf routeList
                        |> Expect.true "not a sub-sequence"
            , fuzzTest "items to keep from model that are not overwritten in routeList should be a sub-sequence of rearranged editList" <|
                \routeList editList ->
                    rearrange keep routeList editList
                        |> List.Extra.isSubsequenceOf
                            (editList |> List.filter (keepAndNotInList routeList))
                        |> Expect.true "not a sub-sequence"
            , fuzzTest "rearranged editList should have as many items as there are relevant items in routeList and editList" <|
                \routeList editList ->
                    rearrange keep routeList editList
                        |> List.length
                        |> Expect.equal
                            (List.length routeList
                                + List.length (editList |> List.filter (keepAndNotInList routeList))
                            )
            , fuzzTest "rearranged editList should not have duplicated keys" <|
                \routeList editList ->
                    rearrange keep routeList editList
                        |> List.Extra.allDifferentBy Tuple.first
                        |> Expect.true "rearranged editList has more than one item for the same key"
            , fuzzTest "items to keep from editList should not occur early in rearranged editList" <|
                \routeList editList ->
                    let
                        rearrangedEditList =
                            -- List.reverse <| -- For testing the test: provoke failures
                            rearrange keep routeList editList
                    in
                    expectationList
                        (List.Extra.lift2
                            (\( ( m1key, _ ), ( m2key, _ ) ) ( ( n1key, _ ) as n1, ( n2key, _ ) ) ->
                                if
                                    keep n1
                                        && (m1key == n2key)
                                        && (m2key == n1key)
                                then
                                    Expect.fail <|
                                        "Item-to-keep "
                                            ++ m2key
                                            ++ " found before item "
                                            ++ m1key
                                            ++ " in rearranged editList ["
                                            ++ String.join ","
                                                (List.map Tuple.first rearrangedEditList)
                                            ++ "]"

                                else
                                    Expect.pass
                            )
                            (List.Extra.uniquePairs editList)
                            (List.Extra.uniquePairs rearrangedEditList)
                        )
            ]
        ]


expectResultFromTestModelAndGivenRoute : TestList -> TestList -> () -> Expect.Expectation
expectResultFromTestModelAndGivenRoute routeList expectedRearrangedEditList =
    \() ->
        rearrange keep routeList testModel
            |> Expect.equal expectedRearrangedEditList


keepAndNotInList : TestList -> TestItem -> Bool
keepAndNotInList list item =
    keep item
        && not (List.any (\x -> Tuple.first x == Tuple.first item) list)


fuzzTest : String -> (TestList -> TestList -> Expect.Expectation) -> Test
fuzzTest =
    fuzz2
        fuzzerRouteList
        fuzzerEditList


fuzzerRouteList : Fuzzer TestList
fuzzerRouteList =
    fuzzerKeyName
        |> Fuzz.map (\key -> ( key, key ++ "n" ))
        |> shortListWithFactorUniqueBy Tuple.first 0.8 6


fuzzerEditList : Fuzzer TestList
fuzzerEditList =
    Fuzz.map2
        (\key empty -> ( key, empty |> Utils.ifElse "" key ))
        fuzzerKeyName
        Fuzz.bool
        |> shortListWithFactorUniqueBy Tuple.first 0.8 7


fuzzerKeyName : Fuzzer String
fuzzerKeyName =
    [ "A", "B", "C", "D", "E", "F" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
