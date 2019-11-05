module Tests.Utils exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List.Extra
import Test exposing (..)
import TestUtils exposing (..)
import Utils


suite : Test
suite =
    describe "Utils"
        [ describe "findAdjacent"
            [ describe "empty list"
                [ test "always True on empty list" <|
                    \() ->
                        Expect.equal (Utils.findAdjacent (always True) []) Nothing
                , test "always False on on empty list" <|
                    \() ->
                        Expect.equal (Utils.findAdjacent (always False) []) Nothing
                ]
            , describe "singleton list"
                [ test "find existing element" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 1) [ 1 ])
                            (Just ( Nothing, 1, Nothing ))
                , test "find non-existing element" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 2) [ 1 ])
                            Nothing
                ]
            , describe "[ 1, 1 ]"
                [ test "find (== 1)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 1) [ 1, 1 ])
                            (Just ( Nothing, 1, Just 1 ))
                , test "find (== 2)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 2) [ 1, 1 ])
                            Nothing
                ]
            , describe "[ 1, 2 ]"
                [ test "find (== 1)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 1) [ 1, 2 ])
                            (Just ( Nothing, 1, Just 2 ))
                , test "find (== 2)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 2) [ 1, 2 ])
                            (Just ( Just 1, 2, Nothing ))
                , test "find non-existing element" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 3) [ 1, 2 ])
                            Nothing
                ]
            , describe "[ 1, 2, 3, 2, 4 ]"
                [ test "find always True" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent (always True) [ 1, 2, 3, 2, 4 ])
                            (Just ( Nothing, 1, Just 2 ))
                , test "find always False" <|
                    \() ->
                        Expect.equal (Utils.findAdjacent (always False) [ 1, 2, 3, 2, 4 ])
                            Nothing
                , test "find (== 1)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 1) [ 1, 2, 3, 2, 4 ])
                            (Just ( Nothing, 1, Just 2 ))
                , test "find (== 2)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 2) [ 1, 2, 3, 2, 4 ])
                            (Just ( Just 1, 2, Just 3 ))
                , test "find (== 3)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 3) [ 1, 2, 3, 2, 4 ])
                            (Just ( Just 2, 3, Just 2 ))
                , test "find (== 4)" <|
                    \() ->
                        Expect.equal
                            (Utils.findAdjacent ((==) 4) [ 1, 2, 3, 2, 4 ])
                            (Just ( Just 2, 4, Nothing ))
                ]
            , fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "check if random value is found in random list" <|
                \v l ->
                    Expect.equal
                        (Utils.findAdjacent ((==) v) l /= Nothing)
                        (List.member v l)
            , describe "compare with alternative implementation"
                [ fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "find random values in random lists" <|
                    \v l ->
                        Expect.equal
                            (Utils.findAdjacent ((==) v) l)
                            (findAdjacentAlternativeImplementation ((==) v) l)
                , fuzz (Fuzz.list Fuzz.int) "find values by a numeric predicate in random lists" <|
                    \l ->
                        Expect.equal
                            (Utils.findAdjacent (\v -> modBy 8 v == 1) l)
                            (findAdjacentAlternativeImplementation (\v -> modBy 8 v == 1) l)
                ]
            ]
        , describe "findMap" <|
            let
                add100IfEven x =
                    if x // 2 * 2 == x then
                        Just (x + 100)

                    else
                        Nothing
            in
            [ test "on empty list" <|
                \() ->
                    Expect.equal (Utils.findMap add100IfEven []) Nothing
            , describe "add100IfEven"
                [ test "[ 1, 2, 3, 4, 5 ]" <|
                    \() ->
                        Expect.equal
                            (Utils.findMap add100IfEven [ 1, 2, 3, 4, 5 ])
                            (Just 102)
                , test "[ 1, 3, 5, 7, 9 ]" <|
                    \() ->
                        Expect.equal
                            (Utils.findMap add100IfEven [ 1, 3, 5, 7, 9 ])
                            Nothing
                ]
            ]
        , describe "lexicalOrder"
            [ fuzz2 Fuzz.string Fuzz.string "order of random strings should match the order of the corresponding lists of chars" <|
                \strL strR ->
                    Utils.lexicalOrder
                        compare
                        (String.toList strL)
                        (String.toList strR)
                        |> Expect.equal (compare strL strR)
            , testOrderingProperties
                "with fuzzy list (short)"
                (shortList 3 (Fuzz.intRange 0 2))
                (Utils.lexicalOrder compare)
            , testOrderingProperties
                "with fuzzy list (regular)"
                (Fuzz.list Fuzz.int)
                (Utils.lexicalOrder compare)
            ]
        ]



{- Use this alternative implementation for testing correctness
   Benchmarking shows that this alternative is about 50% slower than our main implementation (Elm 0.18.0).
-}


findAdjacentAlternativeImplementation : (a -> Bool) -> List a -> Maybe ( Maybe a, a, Maybe a )
findAdjacentAlternativeImplementation predicate list =
    case List.Extra.findIndex predicate list of
        Just i ->
            case List.Extra.getAt i list of
                Just el ->
                    Just
                        ( List.Extra.getAt (i - 1) list
                        , el
                        , List.Extra.getAt (i + 1) list
                        )

                Nothing ->
                    {- Cannot happen -}
                    Nothing

        Nothing ->
            Nothing
