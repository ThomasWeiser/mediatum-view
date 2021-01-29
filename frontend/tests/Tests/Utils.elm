module Tests.Utils exposing (suite)

import Expect
import Fuzz
import Test exposing (..)
import TestUtils exposing (..)
import Utils


suite : Test
suite =
    describe "Utils"
        [ describe "lexicalOrder"
            [ fuzz2 Fuzz.string Fuzz.string "order of random strings should match the order of the corresponding lists of chars" <|
                \strL strR ->
                    Utils.lexicalOrdering
                        compare
                        (String.toList strL)
                        (String.toList strR)
                        |> Expect.equal (compare strL strR)
            , testOrderingProperties
                "with fuzzy list (short)"
                (shortList 3 (Fuzz.intRange 0 2))
                (Utils.lexicalOrdering compare)
            , testOrderingProperties
                "with fuzzy list (regular)"
                (Fuzz.list Fuzz.int)
                (Utils.lexicalOrdering compare)
            ]
        , describe "maybeOrder"
            [ testOrderingProperties
                "with maybe fuzzy element"
                (Fuzz.maybe (Fuzz.intRange 0 2))
                (Utils.maybeOrdering compare)
            ]
        ]
