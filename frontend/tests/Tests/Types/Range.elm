module Tests.Types.Range exposing (fuzzerRange, suite)

import Basics.Extra
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TestUtils exposing (..)
import Types.Range as Range exposing (Range)


suite : Test
suite =
    describe "Types.Range"
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
