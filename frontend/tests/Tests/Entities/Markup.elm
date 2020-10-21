module Tests.Entities.Markup exposing (all)

import Entities.Markup exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


all : Test
all =
    describe "Entities.Markup"
        [ describe "empty"
            [ testText "" <|
                Expect.equal empty
            ]
        , describe "plainText"
            [ testText "" <|
                plainText
                    >> Expect.equal ""
            , testText "foo <mediatum:fts>bar</mediatum:fts> baz" <|
                plainText
                    >> Expect.equal "foo bar baz"
            ]
        , describe "normalizeYear"
            [ testText "" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal ""
            , testText "2020-00-00T00:00:00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020"
            , testText "<mediatum:fts>2020</mediatum:fts>-00-00T00:00:00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020"
            , testText "<mediatum:fts>2020-00-00</mediatum:fts>T00:00:00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020"
            , testText "2020-00-<mediatum:fts>00</mediatum:fts>T00:00:00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020"
            ]
        ]


{-| Use a string as the test name as well as the test subject.
-}
testText : String -> (Markup -> Expect.Expectation) -> Test
testText textToParse expectationOnMarkup =
    test ("text = \"" ++ textToParse ++ "\"") <|
        \_ ->
            parse textToParse
                |> expectationOnMarkup
