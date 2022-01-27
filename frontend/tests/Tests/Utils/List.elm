module Tests.Utils.List exposing (suite)

import Expect
import Fuzz
import List.Extra
import Test exposing (..)
import TestUtils exposing (..)
import Utils
import Utils.List


suite : Test
suite =
    describe "Utils.List"
        [ describe "findAdjacent"
            [ describe "empty list"
                [ test "always True on empty list" <|
                    \() ->
                        Expect.equal (Utils.List.findAdjacent (always True) []) Nothing
                , test "always False on on empty list" <|
                    \() ->
                        Expect.equal (Utils.List.findAdjacent (always False) []) Nothing
                ]
            , describe "singleton list"
                [ test "find existing element" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 1) [ 1 ])
                            (Just ( Nothing, 1, Nothing ))
                , test "find non-existing element" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 2) [ 1 ])
                            Nothing
                ]
            , describe "[ 1, 1 ]"
                [ test "find (== 1)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 1) [ 1, 1 ])
                            (Just ( Nothing, 1, Just 1 ))
                , test "find (== 2)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 2) [ 1, 1 ])
                            Nothing
                ]
            , describe "[ 1, 2 ]"
                [ test "find (== 1)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 1) [ 1, 2 ])
                            (Just ( Nothing, 1, Just 2 ))
                , test "find (== 2)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 2) [ 1, 2 ])
                            (Just ( Just 1, 2, Nothing ))
                , test "find non-existing element" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 3) [ 1, 2 ])
                            Nothing
                ]
            , describe "[ 1, 2, 3, 2, 4 ]"
                [ test "find always True" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent (always True) [ 1, 2, 3, 2, 4 ])
                            (Just ( Nothing, 1, Just 2 ))
                , test "find always False" <|
                    \() ->
                        Expect.equal (Utils.List.findAdjacent (always False) [ 1, 2, 3, 2, 4 ])
                            Nothing
                , test "find (== 1)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 1) [ 1, 2, 3, 2, 4 ])
                            (Just ( Nothing, 1, Just 2 ))
                , test "find (== 2)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 2) [ 1, 2, 3, 2, 4 ])
                            (Just ( Just 1, 2, Just 3 ))
                , test "find (== 3)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 3) [ 1, 2, 3, 2, 4 ])
                            (Just ( Just 2, 3, Just 2 ))
                , test "find (== 4)" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) 4) [ 1, 2, 3, 2, 4 ])
                            (Just ( Just 2, 4, Nothing ))
                ]
            , fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "check if random value is found in random list" <|
                \v l ->
                    Expect.equal
                        (Utils.List.findAdjacent ((==) v) l /= Nothing)
                        (List.member v l)
            , describe "compare with alternative implementation"
                [ fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "find random values in random lists" <|
                    \v l ->
                        Expect.equal
                            (Utils.List.findAdjacent ((==) v) l)
                            (findAdjacentAlternativeImplementation ((==) v) l)
                , fuzz (Fuzz.list Fuzz.int) "find values by a numeric predicate in random lists" <|
                    \l ->
                        Expect.equal
                            (Utils.List.findAdjacent (\v -> modBy 8 v == 1) l)
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
                    Expect.equal (Utils.List.findMap add100IfEven []) Nothing
            , describe "add100IfEven"
                [ test "[ 1, 2, 3, 4, 5 ]" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findMap add100IfEven [ 1, 2, 3, 4, 5 ])
                            (Just 102)
                , test "[ 1, 3, 5, 7, 9 ]" <|
                    \() ->
                        Expect.equal
                            (Utils.List.findMap add100IfEven [ 1, 3, 5, 7, 9 ])
                            Nothing
                ]
            ]
        , describe "setOnMapping" <|
            let
                setOnMod3 : Int -> List Int -> List Int
                setOnMod3 =
                    Utils.List.setOnMapping (\n -> n |> modBy 3)
            in
            [ test "adds value to empty list" <|
                \() ->
                    Expect.equal (setOnMod3 7 []) [ 7 ]
            , test "replaces all matching values" <|
                \() ->
                    Expect.equal
                        (setOnMod3 7 [ 0, 1, 2, 3, 4, 5, 6, 4, 5, 6 ])
                        [ 0, 7, 2, 3, 7, 5, 6, 7, 5, 6 ]
            , test "adds value to end of list if no element matches on the mapping" <|
                \() ->
                    Expect.equal
                        (setOnMod3 7 [ 0, 2, 3, 2, 5 ])
                        [ 0, 2, 3, 2, 5, 7 ]
            , fuzz2 Fuzz.int (Fuzz.list Fuzz.int) "compare with alternative implementation" <|
                \i l ->
                    Expect.equal
                        (setOnMod3 i l)
                        (setOnMappingAlternativeImplementation (\n -> n |> modBy 3) i l)
            ]
        , describe "updateOnMapping" <|
            let
                updateOnMod3Is1 : (Maybe Int -> Maybe Int) -> List Int -> List Int
                updateOnMod3Is1 update =
                    Utils.List.updateOnMapping update (\n -> n |> modBy 3) 1

                update666777 : Maybe Int -> Maybe Int
                update666777 maybeX =
                    maybeX == Nothing |> Utils.ifElse (Just 666) (Just 777)
            in
            [ describe "on empty list" <|
                [ test "adds Just value to empty list" <|
                    \() ->
                        Expect.equal
                            (updateOnMod3Is1 update666777 [])
                            [ 666 ]
                , test "adds no element on identity" <|
                    \() ->
                        Expect.equal (updateOnMod3Is1 identity []) []
                ]
            , test "replaces all matching values" <|
                \() ->
                    Expect.equal
                        (updateOnMod3Is1 update666777 [ 0, 1, 2, 3, 4, 5, 6, 4, 5, 6, 7, 8 ])
                        [ 0, 777, 2, 3, 777, 5, 6, 777, 5, 6, 777, 8 ]
            , test "conditionally replaces all matching values" <|
                \() ->
                    Expect.equal
                        (updateOnMod3Is1
                            (\maybeX ->
                                case maybeX of
                                    Just x ->
                                        x < 3 |> Utils.ifElse (Just (x + 100)) (Just (x + 200))

                                    Nothing ->
                                        Just -1
                            )
                            [ 0, 1, 2, 3, 4, 5, 6, 4, 5, 6, 7, 8 ]
                        )
                        [ 0, 101, 2, 3, 204, 5, 6, 204, 5, 6, 207, 8 ]
            , test "adds value to end of list if no element matches on the mapping" <|
                \() ->
                    Expect.equal
                        (updateOnMod3Is1 (always (Just 7)) [ 0, 2, 3, 2, 5 ])
                        [ 0, 2, 3, 2, 5, 7 ]
            , test "adds nothing to end of list if no element matches on the mapping" <|
                \() ->
                    Expect.equal
                        (updateOnMod3Is1 (always Nothing) [ 0, 2, 3, 2, 5 ])
                        [ 0, 2, 3, 2, 5 ]
            ]
        , describe "mapWhile" <|
            let
                exampleMapping x =
                    if x < 5 then
                        Just (x * 10)

                    else
                        Nothing
            in
            [ test "on empty list" <|
                \() ->
                    Expect.equal (Utils.List.mapWhile exampleMapping []) ( True, [] )
            , test "mixed list" <|
                \() ->
                    Expect.equal (Utils.List.mapWhile exampleMapping [ 1, 2, 5, 6, 3, 4, 7, 8 ]) ( False, [ 10, 20 ] )
            , test "starting with a mapping to Nothing" <|
                \() ->
                    Expect.equal (Utils.List.mapWhile exampleMapping [ 5, 6, 3, 4, 7, 8 ]) ( False, [] )
            , test "works for very long list (i.e. is call stack size safe), dropping out early" <|
                \() ->
                    Expect.equal
                        (Utils.List.mapWhile exampleMapping (List.range 1 100000))
                        ( False, [ 10, 20, 30, 40 ] )
            , test "works for very long list (i.e. is call stack size safe), not dropping out" <|
                \() ->
                    Expect.equal
                        (Utils.List.mapWhile exampleMapping (List.range -100000 -1))
                        ( True, List.map ((*) 10) (List.range -100000 -1) )
            ]
        , describe "mapEllipsis" <|
            let
                exampleMapping x =
                    if x < 5 then
                        Just (x * 10)

                    else
                        Nothing
            in
            [ test "on empty list" <|
                \() ->
                    Expect.equal (Utils.List.mapEllipsis 99 exampleMapping []) []
            , test "mixed list" <|
                \() ->
                    Expect.equal (Utils.List.mapEllipsis 99 exampleMapping [ 1, 2, 5, 6, 3, 4, 7, 8 ]) [ 10, 20, 99 ]
            , test "starting with a mapping to Nothing" <|
                \() ->
                    Expect.equal (Utils.List.mapEllipsis 99 exampleMapping [ 5, 6, 3, 4, 7, 8 ]) [ 99 ]
            , test "works for very long list (i.e. is call stack size safe), dropping out early" <|
                \() ->
                    Expect.equal
                        (Utils.List.mapEllipsis 99 exampleMapping (List.range 1 100000))
                        [ 10, 20, 30, 40, 99 ]
            , test "works for very long list (i.e. is call stack size safe), not dropping out" <|
                \() ->
                    Expect.equal
                        (Utils.List.mapEllipsis 99 exampleMapping (List.range -100000 -1))
                        (List.map ((*) 10) (List.range -100000 -1))
            ]
        , describe "lexicalOrder"
            [ fuzz2 Fuzz.string Fuzz.string "order of random strings should match the order of the corresponding lists of chars" <|
                \strL strR ->
                    Utils.List.lexicalOrdering
                        compare
                        (String.toList strL)
                        (String.toList strR)
                        |> Expect.equal (compare strL strR)
            , testFineOrderingProperties
                "with fuzzy list (short)"
                (shortList 3 (Fuzz.intRange 0 2))
                (Utils.List.lexicalOrdering compare)
            , testFineOrderingProperties
                "with fuzzy list (regular)"
                (Fuzz.list Fuzz.int)
                (Utils.List.lexicalOrdering compare)
            ]
        , let
            f isLast x =
                if isLast then
                    x * 4

                else
                    x * 2
          in
          describe "mapAndMarkLast"
            [ test "on empty list" <|
                \() ->
                    Utils.List.mapAndMarkLast f []
                        |> Expect.equal []
            , test "one element" <|
                \() ->
                    Utils.List.mapAndMarkLast f [ 10 ]
                        |> Expect.equal [ 40 ]
            , test "two elements" <|
                \() ->
                    Utils.List.mapAndMarkLast f [ 10, 11 ]
                        |> Expect.equal [ 20, 44 ]
            , test "four elements" <|
                \() ->
                    Utils.List.mapAndMarkLast f [ 10, 11, 12, 13 ]
                        |> Expect.equal [ 20, 22, 24, 52 ]
            ]
        ]



{- Use this alternative implementation for testing correctness
   Benchmarking shows that this alternative is about 50% slower than our main implementation (Elm 0.18.0).
-}


findAdjacentAlternativeImplementation : (a -> Bool) -> List a -> Maybe ( Maybe a, a, Maybe a )
findAdjacentAlternativeImplementation predicate list =
    List.Extra.findIndex predicate list
        |> Maybe.andThen
            (\i ->
                List.Extra.getAt i list
                    |> Maybe.map
                        (\el ->
                            ( List.Extra.getAt (i - 1) list
                            , el
                            , List.Extra.getAt (i + 1) list
                            )
                        )
            )



{- Alternative recursive implementation of setOnMapping.
   It's not tail recursive, so not suitable for large lists.
-}


setOnMappingAlternativeImplementation : (a -> b) -> a -> List a -> List a
setOnMappingAlternativeImplementation mapping replacement =
    let
        walk found list =
            case list of
                [] ->
                    if found then
                        []

                    else
                        [ replacement ]

                element :: rest ->
                    if mapping element == mapping replacement then
                        replacement :: walk True rest

                    else
                        element :: walk found rest
    in
    walk False
