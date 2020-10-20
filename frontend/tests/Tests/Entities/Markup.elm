module Tests.Entities.Markup exposing (all)

import Entities.Markup exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


all : Test
all =
    describe "Utils.Markup"
        [ describe "without valid markup"
            [ testText ""
                []
            , testText "foobar"
                [ Text "foobar" ]
            , testText "foo<mediatum:unknown>bar</mediatum:unknown>baz"
                [ Text "foo<mediatum:unknown>bar</mediatum:unknown>baz" ]
            , testText "foo<mediatum:unknown/>bar"
                [ Text "foo<mediatum:unknown/>bar" ]
            , testText "foo<mediatum:unknown>bar"
                [ Text "foo<mediatum:unknown>bar" ]
            ]
        , describe "with markup"
            [ testText "<mediatum:fts>bar</mediatum:fts>"
                [ Fts "bar" ]
            , testText "<mediatum:fts></mediatum:fts>"
                []
            , testText "foo<mediatum:fts></mediatum:fts><mediatum:fts></mediatum:fts>baz"
                [ Text "foo", Text "baz" ]
            , testText "foo<mediatum:fts>bar</mediatum:fts>baz"
                [ Text "foo", Fts "bar", Text "baz" ]
            , testText "<foo<<mediatum:fts><bar<</mediatum:fts><baz<"
                [ Text "<foo<", Fts "<bar<", Text "<baz<" ]
            , testText "foo<mediatum:fts>bar1</mediatum:fts><mediatum:fts>bar2</mediatum:fts>baz"
                [ Text "foo", Fts "bar1", Fts "bar2", Text "baz" ]
            ]
        , describe "bad nesting"
            [ testText "<mediatum:fts><mediatum:fts>bar</mediatum:fts>"
                [ Fts "<mediatum:fts>bar" ]
            , testText "<mediatum:fts>bar</mediatum:fts></mediatum:fts>"
                [ Fts "bar", Text "</mediatum:fts>" ]
            ]
        , fuzz
            fuzzerText
            "Random texts should always parse without an error"
            (parseTestable >> Expect.ok)
        ]


{-| Use a string as the test name as well as the test subject.
-}
testText : String -> Segments -> Test
testText textToParse expectedResult =
    test ("text = \"" ++ textToParse ++ "\"") <|
        \_ ->
            parseTestable textToParse
                |> Expect.equal (Ok expectedResult)


fuzzerText : Fuzzer String
fuzzerText =
    [ " ", "a", "<", ">", "<mediatum:unknown>", "<mediatum:fts>", "</mediatum:fts>" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
        |> Fuzz.list
        |> Fuzz.map String.concat
