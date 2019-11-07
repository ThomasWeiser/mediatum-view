module Tests.Range exposing (fuzzerRange, suite)

import Basics.Extra
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Range exposing (Range)
import Test exposing (..)
import TestUtils exposing (..)


suite : Test
suite =
    describe "Data.Ordering"
        [ testOrderingProperties
            "Range"
            (fuzzerRange (Fuzz.intRange 0 3))
            Range.compare
        ]


fuzzerRange : Fuzzer comparable -> Fuzzer (Range comparable)
fuzzerRange fuzzerValue =
    Fuzz.oneOf
        [ Fuzz.map Range.From fuzzerValue
        , Fuzz.map Range.To fuzzerValue
        , Fuzz.map2 (Basics.Extra.curry Range.fromTo)
            fuzzerValue
            fuzzerValue
        ]
