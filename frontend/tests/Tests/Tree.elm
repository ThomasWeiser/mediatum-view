module Tests.Tree exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree


suite : Test
suite =
    describe "Tree"
        [ test "dummy test, always passes"
            (always Expect.pass)
        ]
