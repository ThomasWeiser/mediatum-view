module Tests.App exposing (suite)

import App
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    describe "App"
        [ test "dummy test, always passes"
            (always Expect.pass)
        ]
