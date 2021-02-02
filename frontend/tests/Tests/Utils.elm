module Tests.Utils exposing (suite)

import Fuzz
import Test exposing (..)
import TestUtils exposing (..)
import Utils


suite : Test
suite =
    describe "Utils"
        [ describe "maybeOrder"
            [ testOrderingProperties
                "with maybe fuzzy element"
                (Fuzz.maybe (Fuzz.intRange 0 2))
                (Utils.maybeOrdering compare)
            ]
        ]
