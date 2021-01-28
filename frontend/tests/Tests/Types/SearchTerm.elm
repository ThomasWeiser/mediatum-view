module Tests.Types.SearchTerm exposing (fuzzerSearchTerm, suite)

import Expect
import Fuzz exposing (Fuzzer)
import String.Extra
import Test exposing (..)
import TestUtils exposing (..)
import Types.SearchTerm as SearchTerm exposing (SearchTerm)


suite : Test
suite =
    describe "Types.SearchTerm"
        [ testOrderingProperties "SearchTerm" fuzzerSearchTerm SearchTerm.ordering
        , fuzz Fuzz.string "Random strings" <|
            \inputString ->
                case SearchTerm.fromString inputString |> Maybe.map SearchTerm.toString of
                    Just resultString ->
                        resultString
                            |> Expect.all
                                [ String.isEmpty >> Expect.false "Expected search term to be not empty"
                                , String.Extra.isBlank >> Expect.false "Expected search term to be not blank"
                                , fixpoint String.Extra.clean
                                ]

                    Nothing ->
                        Expect.pass
        ]


{-| A fuzzer for canonical search terms,
i.e. without whitespace at the left and the right
and without repeated whitespace within the string.

It generates some common search string formats as well as random search strings.

-}
fuzzerSearchTerm : Fuzzer SearchTerm
fuzzerSearchTerm =
    Fuzz.oneOf
        [ [ "foo"
          , "foo bar"
          , "\"foo\""
          , "\"foo bar\""
          , "\"foo bar\" baz"
          , "-foo"
          , "foo -bar -baz"
          , "-\"foo bar\" baz"
          ]
            |> List.map Fuzz.constant
            |> Fuzz.oneOf
        , Fuzz.string
            |> Fuzz.map
                (String.filter ((/=) '\n'))
        ]
        |> Fuzz.map
            (SearchTerm.fromStringWithDefault "baz")
