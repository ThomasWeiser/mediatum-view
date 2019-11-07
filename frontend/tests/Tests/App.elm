module Tests.App exposing (suite)

import App
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "App"
        [ test "dummy test, always passes"
            (always Expect.pass)
        ]
