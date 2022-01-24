module Tests.Entities.Markup exposing (all)

import Entities.Markup exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import String.Extra
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
            , testText "foo <span>bar</span> baz" <|
                plainText
                    >> Expect.equal "foo bar baz"
            ]
        , describe "toStrings"
            [ testText "" <|
                toHtmlString
                    >> Expect.equal ""
            , testText "foo <span>bar</span> baz" <|
                toHtmlString
                    >> Expect.equal "foo <span>bar</span> baz"
            , testText "foo <span>bar" <|
                toHtmlString
                    >> Expect.equal "<div class=\"unparsable\">foo <span>bar</div>"
            ]
        , describe "String.Extra.softBreak behavior demo"
            [ test "String.Extra.softBreak A 7" <|
                \_ ->
                    String.Extra.softBreak 7 "abcdefghi jklmnopqrst"
                        |> Expect.equal [ "abcdefghi ", "jklmnopqrst" ]
            , test "String.Extra.softBreak A 12" <|
                \_ ->
                    String.Extra.softBreak 12 "abcdefghi jklmnopqrst"
                        |> Expect.equal [ "abcdefghi ", "jklmnopqrst" ]
            , test "String.Extra.softBreak B 1" <|
                \_ ->
                    String.Extra.softBreak 1 "ab cd ef gh ij kl mo"
                        |> Expect.equal [ "ab ", "cd ", "ef ", "gh ", "ij ", "kl ", "mo" ]
            , test "String.Extra.softBreak B 2" <|
                \_ ->
                    String.Extra.softBreak 2 "ab cd ef gh ij kl mo"
                        |> Expect.equal [ "ab ", "cd ", "ef ", "gh ", "ij ", "kl ", "mo" ]
            , test "String.Extra.softBreak B 3" <|
                \_ ->
                    String.Extra.softBreak 3 "ab cd ef gh ij kl mo"
                        |> Expect.equal [ "ab ", "cd ", "ef ", "gh ", "ij ", "kl ", "mo" ]
            , test "String.Extra.softBreak B 4" <|
                \_ ->
                    String.Extra.softBreak 4 "ab cd ef gh ij kl mo"
                        |> Expect.equal [ "ab ", "cd ", "ef ", "gh ", "ij ", "kl ", "mo" ]
            , test "String.Extra.softBreak B 5" <|
                \_ ->
                    String.Extra.softBreak 5 "ab cd ef gh ij kl mo"
                        |> Expect.equal [ "ab cd ", "ef gh ", "ij kl ", "mo" ]
            , test "String.Extra.softBreak B 6" <|
                \_ ->
                    String.Extra.softBreak 6 "ab cd ef gh ij kl mo"
                        |> Expect.equal [ "ab cd ", "ef gh ", "ij kl ", "mo" ]
            , test "String.Extra.softBreak B 7" <|
                \_ ->
                    String.Extra.softBreak 7 "ab cd ef gh ij kl mo"
                        |> Expect.equal [ "ab cd ", "ef gh ", "ij kl ", "mo" ]
            , test "String.Extra.softBreak C 1" <|
                \_ ->
                    String.Extra.softBreak 7 " abcd efgh "
                        |> Expect.equal [ " abcd ", "efgh " ]
            , -- Issue with String.Extra.softBreak, eating the initial whitespace if truncuating:
              -- We use a workaround in `trim`.
              -- TODO: Report upstream to elm-community/string-extra
              test "String.Extra.softBreak C 2" <|
                \_ ->
                    String.Extra.softBreak 7 " abcdefgh "
                        |> Expect.equal [ "abcdefgh " ]
            , test "String.Extra.softBreak C 3" <|
                \_ ->
                    String.Extra.softBreak 3 "  ab  cd  ef  gh  "
                        |> Expect.equal [ "  ", "ab  ", "cd  ", "ef  ", "gh  " ]
            ]
        , describe "trim 0"
            [ testText "" <|
                trim 0
                    >> toHtmlString
                    >> Expect.equal ""
            , testText " " <|
                trim 0
                    >> toHtmlString
                    >> Expect.equal ""
            , testText "a" <|
                trim 0
                    >> toHtmlString
                    >> Expect.equal ""
            ]
        , describe "trim flat"
            [ testText "" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal ""
            , testText "abcdefghijkl" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">abcdefghijkl</span> ..."
            , testText "abcdefghi jklmnopqrst" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">abcdefghi </span> ..."
            , testText "abcdefghijklmnopqrst" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">abcdefghijklmn</span> ..."
            , testText "ab cd ef gh ij kl mn op" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">ab cd </span> ..."
            , testText "*b cd ef gh ij kl mn op" <|
                trim 23
                    >> toHtmlString
                    >> Expect.equal "*b cd ef gh ij kl mn op"
            ]
        , describe "trim nested shallow"
            [ testText "1b<u>cd</u>ef<v>cd</v>ij<x>kl</x>mn<y>kl</y>" <|
                trim 1
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">1b</span> ..."
            , testText "3b<u>cd</u>ef<v>cd</v>ij<x>kl</x>mn<y>kl</y>" <|
                trim 3
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">3b<u>cd</u></span> ..."
            , testText "5b<u>cd</u>ef<v>cd</v>ij<x>kl</x>mn<y>kl</y>" <|
                trim 5
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">5b<u>cd</u>ef</span> ..."
            , testText "7b<u>cd</u>ef<v>cd</v>ij<x>kl</x>mn<y>kl</y>" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">7b<u>cd</u>ef<v>cd</v></span> ..."
            , testText "*b<u>cd</u>ef<v>cd</v>ij<x>kl</x>mn<y>kl</y>" <|
                trim 16
                    >> toHtmlString
                    >> Expect.equal "*b<u>cd</u>ef<v>cd</v>ij<x>kl</x>mn<y>kl</y>"
            ]
        , describe "trim nested deep"
            [ testText "7b<u>cd<v>ef<w>gh</w>ij</v>kl</u>mn" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\">7b<u>cd<v>ef<w>gh</w></v></u></span> ..."
            , testText "*b<u>cd<v>ef<w>gh</w>ij</v>kl</u>mn" <|
                trim 14
                    >> toHtmlString
                    >> Expect.equal "*b<u>cd<v>ef<w>gh</w>ij</v>kl</u>mn"
            ]
        , describe "trim regression test, working around String.Extra.softBreak bug"
            [ testText "<span><span class=\"highlight-search-term\">von</span> Soosten, Per</span>" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\"><span><span class=\"highlight-search-term\">von</span> Soosten, </span></span> ..."
            , testText "<span class=\"highlight-search-term\">von</span> Soosten, Per" <|
                trim 7
                    >> toHtmlString
                    >> Expect.equal "<span class=\"with-ellipisis\"><span class=\"highlight-search-term\">von</span> Soosten, </span> ..."
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
            , testText "2020-00-00T00:00:00Z" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020"
            , testText "2020-00-00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020"
            , testText "2020-11-22T11:22:33" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020"
            , testText "2020-00-00, 2021-00-00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020, 2021"
            , testText "abc<span>2020-00-00T00:00:00</span>def" <|
                normalizeYear
                    >> toHtmlString
                    >> Expect.equal "abc<span>2020</span>def"
            , testText "2020-00-00T00:00:00missing word boundary" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020-00-00T00:00:00missing word boundary"
            , testText "2020-00-00missing word boundary" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "2020-00-00missing word boundary"
            , testText "missing word boundary2020-00-00T00:00:00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "missing word boundary2020-00-00T00:00:00"
            , testText "missing word boundary2020-00-00" <|
                normalizeYear
                    >> plainText
                    >> Expect.equal "missing word boundary2020-00-00"
            ]
        , describe "normalizeYearMonth"
            [ testText "" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal ""
            , testText "2020-11-00T00:00:00" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "2020-11"
            , testText "2020-11-00T00:00:00Z" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "2020-11"
            , testText "2020-11-00" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "2020-11"
            , testText "2020-11-22T11:22:33" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "2020-11"
            , testText "2020-11-00, 2021-12-00" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "2020-11, 2021-12"
            , testText "abc<span>2020-11-00T00:00:00</span>def" <|
                normalizeYearMonth
                    >> toHtmlString
                    >> Expect.equal "abc<span>2020-11</span>def"
            , testText "2020-11-00T00:00:00missing word boundary" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "2020-11-00T00:00:00missing word boundary"
            , testText "2020-11-00missing word boundary" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "2020-11-00missing word boundary"
            , testText "missing word boundary2020-11-00T00:00:00" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "missing word boundary2020-11-00T00:00:00"
            , testText "missing word boundary2020-11-00" <|
                normalizeYearMonth
                    >> plainText
                    >> Expect.equal "missing word boundary2020-11-00"
            ]
        , describe "normalizeYearMonthDay"
            [ testText "" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal ""
            , testText "2020-11-22T00:00:00" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "2020-11-22"
            , testText "2020-11-22T00:00:00Z" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "2020-11-22"
            , testText "2020-11-22" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "2020-11-22"
            , testText "2020-11-22T11:22:33" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "2020-11-22"
            , testText "2020-11-22, 2021-12-24" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "2020-11-22, 2021-12-24"
            , testText "abc<span>2020-11-22T00:00:00</span>def" <|
                normalizeYearMonthDay
                    >> toHtmlString
                    >> Expect.equal "abc<span>2020-11-22</span>def"
            , testText "2020-11-22T00:00:00missing word boundary" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "2020-11-22T00:00:00missing word boundary"
            , testText "2020-11-22missing word boundary" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "2020-11-22missing word boundary"
            , testText "missing word boundary2020-11-22T00:00:00" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "missing word boundary2020-11-22T00:00:00"
            , testText "missing word boundary2020-11-22" <|
                normalizeYearMonthDay
                    >> plainText
                    >> Expect.equal "missing word boundary2020-11-22"
            ]
        ]


{-| Use a string as the test name as well as the test subject.
-}
testText : String -> (Markup -> Expect.Expectation) -> Test
testText textToParse expectationOnMarkup =
    test ("text = \"" ++ textToParse ++ "\"") <|
        \_ ->
            parse (DivClass "unparsable") textToParse
                |> expectationOnMarkup
