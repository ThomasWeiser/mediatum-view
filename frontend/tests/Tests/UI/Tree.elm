module Tests.UI.Tree exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import UI.Tree


suite : Test
suite =
    describe "Tree"
        [ test "dummy test, always passes"
            (always Expect.pass)
        ]
