module Tests.Route.Url exposing (suite)

import Data.Types exposing (FtsSorting(..), nodeIdFromInt)
import Expect exposing (Expectation)
import List.Nonempty exposing (Nonempty)
import Route exposing (Route, RouteParameters, RoutePath(..))
import Route.Url
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Route
import Url exposing (Url)
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
                    >> Maybe.andThen Route.Url.parseUrl
                    >> nothing
            , testString "https://example.com/" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> Expect.equal ""
                        , Route.Url.toString >> Expect.equal "/"
                        ]
            , testString "https://example.com/123" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (nodeIdFromInt 123))
                        , .parameters >> .ftsTerm >> Expect.equal ""
                        , .parameters >> .ftsSorting >> Expect.equal Route.defaultFtsSorting
                        , .parameters >> .filterByYear >> nothing
                        , .parameters >> .filterByTitle >> Expect.equal []
                        , Route.Url.toString >> Expect.equal "/123"
                        ]
            , testString "https://example.com/?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> Expect.equal "foo"
                        , Route.Url.toString >> Expect.equal "/?fts-term=foo"
                        ]
            , testString "https://example.com/?fts-sorting=by-rank" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> Expect.equal ""
                        , .parameters >> .ftsSorting >> Expect.equal FtsByRank
                        , Route.Url.toString
                            >> Expect.equal
                                (Route.defaultFtsSorting
                                    == FtsByRank
                                    |> Utils.ifElse "/" "/?fts-sorting=by-rank"
                                )
                        ]
            , testString "https://example.com/?fts-sorting=by-date" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> Expect.equal ""
                        , .parameters >> .ftsSorting >> Expect.equal FtsByDate
                        , Route.Url.toString
                            >> Expect.equal
                                (Route.defaultFtsSorting
                                    == FtsByDate
                                    |> Utils.ifElse "/" "/?fts-sorting=by-date"
                                )
                        ]
            , testString "https://example.com/123/456?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.TwoIds (nodeIdFromInt 123) (nodeIdFromInt 456))
                        , .parameters >> .ftsTerm >> Expect.equal "foo"
                        , Route.Url.toString >> Expect.equal "/123/456?fts-term=foo"
                        ]
            , describe "It should remove search terms that are empty or only whitespace"
                [ testString "https://example.com/?fts-term=%20%20&filter-by-title=&filter-by-title=%20%20" <|
                    Url.fromString
                        >> Maybe.andThen Route.Url.parseUrl
                        >> justAndThenAll
                            [ .path >> Expect.equal Route.NoId
                            , .parameters >> .ftsTerm >> Expect.equal ""
                            , .parameters >> .filterByTitle >> Expect.equal []
                            , Route.Url.toString >> Expect.equal "/"
                            ]
                ]
            , testString "https://example.com/789/?fts%2Dterm=%20foo%20%20bar%20" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (nodeIdFromInt 789))
                        , .parameters >> .ftsTerm >> Expect.equal "foo bar"
                        , Route.Url.toString >> Expect.equal "/789?fts-term=foo%20bar"
                        ]
            , testString "https://example.com/789/?filter-by-title=%20foo%20&filter-by-year=2001-2011&filter-by-title=\"bar%20%20baz\"" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (nodeIdFromInt 789))
                        , .parameters >> .ftsTerm >> Expect.equal ""
                        , .parameters >> .filterByYear >> Expect.equal (Just ( 2001, 2011 ))
                        , .parameters >> .filterByTitle >> Expect.equal [ "foo", "\"bar baz\"" ]
                        , Route.Url.toString >> Expect.equal "/789?filter-by-year=2001-2011&filter-by-title=foo&filter-by-title=%22bar%20baz%22"
                        ]

            {- I guess percent-coding should work within the path, but it doesn't
               Bug: https://github.com/elm/url/issues/16
                  , testString "https://example.com/a78%39" <|
                      Url.fromString
                          >> Maybe.andThen Route.Url.parseUrl
                          >> justAndThenAll
                              [ .path >> Expect.equal (Route.OneId 789)
                              , .parameters >> .ftsTerm >> Expect.equal ""
                              ]
            -}
            ]
        , describe "fuzzing routes"
            [ fuzz Tests.Route.fuzzerRoute
                "building and parsing again results in same route"
                (\route ->
                    route
                        |> Route.Url.toString
                        -- |> Debug.log "toString"
                        |> String.append "https://example.com"
                        |> Url.fromString
                        |> Maybe.andThen Route.Url.parseUrl
                        |> justAndThen (Expect.equal route)
                )
            ]
        ]
