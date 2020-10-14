module Tests.Types.Route.Url exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types.Route
import Types.Id as Id
import Types.Range as Range
import Types.Route as Route
import Types.Route.Url
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (FtsSorting(..))
import Url
import Utils


suite : Test
suite =
    describe "URL segmentation and parsing"
        [ describe "URL segmentation"
            [ testString "https://example.com:443" <|
                Url.fromString
                    >> Expect.equal
                        (Just
                            { protocol = Url.Https
                            , host = "example.com"
                            , port_ = Just 443
                            , path = "/"
                            , query = Nothing
                            , fragment = Nothing
                            }
                        )
            , testString "http://example.com/core/List?q=top%20hat#map" <|
                Url.fromString
                    >> Expect.equal
                        (Just
                            { protocol = Url.Http
                            , host = "example.com"
                            , port_ = Nothing
                            , path = "/core/List"
                            , query = Just "q=top%20hat"
                            , fragment = Just "map"
                            }
                        )
            ]
        , describe "URL parsing and building"
            [ testString "https://example.com/abc" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> nothing
            , testString "https://example.com/" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> nothing
                        , Types.Route.Url.toString >> Expect.equal "/"
                        ]
            , testString "https://example.com/123" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (Id.fromInt 123))
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .ftsSorting >> Expect.equal Route.defaultFtsSorting
                        , .parameters >> .filterByYear >> nothing
                        , .parameters >> .filterByTitle >> nothing
                        , Types.Route.Url.toString >> Expect.equal "/123"
                        ]
            , testString "https://example.com/?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> expectJustSearchTerm "foo"
                        , Types.Route.Url.toString >> Expect.equal "/?fts-term=foo"
                        ]
            , testString "https://example.com/?fts-sorting=by-rank" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .ftsSorting >> Expect.equal FtsByRank
                        , Types.Route.Url.toString
                            >> Expect.equal
                                (Route.defaultFtsSorting
                                    == FtsByRank
                                    |> Utils.ifElse "/" "/?fts-sorting=by-rank"
                                )
                        ]
            , testString "https://example.com/?fts-sorting=by-date" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .ftsSorting >> Expect.equal FtsByDate
                        , Types.Route.Url.toString
                            >> Expect.equal
                                (Route.defaultFtsSorting
                                    == FtsByDate
                                    |> Utils.ifElse "/" "/?fts-sorting=by-date"
                                )
                        ]
            , testString "https://example.com/123/456?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.TwoIds (Id.fromInt 123) (Id.fromInt 456))
                        , .parameters >> .ftsTerm >> expectJustSearchTerm "foo"
                        , Types.Route.Url.toString >> Expect.equal "/123/456?fts-term=foo"
                        ]
            , testString "https://example.com/200/?filter-by-year=2001-2011" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.FromTo 2001 2011))
                        , Types.Route.Url.toString >> Expect.equal "/200?filter-by-year=2001-2011"
                        ]
            , testString "https://example.com/201/?filter-by-year=2011-2001" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.FromTo 2001 2011))
                        , Types.Route.Url.toString >> Expect.equal "/201?filter-by-year=2001-2011"
                        ]
            , testString "https://example.com/202/?filter-by-year=2001-" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.From 2001))
                        , Types.Route.Url.toString >> Expect.equal "/202?filter-by-year=2001-"
                        ]
            , testString "https://example.com/203/?filter-by-year=-2011" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.To 2011))
                        , Types.Route.Url.toString >> Expect.equal "/203?filter-by-year=-2011"
                        ]
            , testString "https://example.com/204/?filter-by-year=-" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal Nothing
                        , Types.Route.Url.toString >> Expect.equal "/204"
                        ]
            , describe "It should remove search terms that are empty or only whitespace"
                [ testString "https://example.com/?fts-term=%20%20&filter-by-title=&filter-by-title=%20%20" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .path >> Expect.equal Route.NoId
                            , .parameters >> .ftsTerm >> nothing
                            , .parameters >> .filterByTitle >> nothing
                            , Types.Route.Url.toString >> Expect.equal "/"
                            ]
                ]
            , testString "https://example.com/789/?fts%2Dterm=%20foo%20%20bar%20" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (Id.fromInt 789))
                        , .parameters >> .ftsTerm >> expectJustSearchTerm "foo bar"
                        , Types.Route.Url.toString >> Expect.equal "/789?fts-term=foo%20bar"
                        ]
            , testString "https://example.com/789/?filter-by-year=2001-2011&filter-by-title=%20foo%20\"bar%20%20baz\"" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (Id.fromInt 789))
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .filterByYear >> Expect.equal (Just (Range.FromTo 2001 2011))
                        , .parameters >> .filterByTitle >> expectJustSearchTerm "foo \"bar baz\""
                        , Types.Route.Url.toString >> Expect.equal "/789?filter-by-year=2001-2011&filter-by-title=foo%20%22bar%20baz%22"
                        ]

            {- I guess percent-coding should work within the path, but it doesn't
               Bug: https://github.com/elm/url/issues/16
                  , testString "https://example.com/a78%39" <|
                      Url.fromString
                          >> Maybe.andThen Types.Route.Url.parseUrl
                          >> justAndThenAll
                              [ .path >> Expect.equal (Route.OneId 789)
                              , .parameters >> .ftsTerm >> nothing
                              ]
            -}
            ]
        , describe "fuzzing routes"
            [ fuzz Tests.Types.Route.fuzzerRoute
                "building and parsing again results in same route"
                (\route ->
                    route
                        |> Types.Route.Url.toString
                        -- |> Debug.log "toString"
                        |> String.append "https://example.com"
                        |> Url.fromString
                        |> Maybe.andThen Types.Route.Url.parseUrl
                        |> justAndThen (Expect.equal route)
                )
            ]
        ]


expectJustSearchTerm : String -> Maybe SearchTerm -> Expectation
expectJustSearchTerm expectedString =
    justAndThen
        (Types.SearchTerm.toString >> Expect.equal expectedString)
