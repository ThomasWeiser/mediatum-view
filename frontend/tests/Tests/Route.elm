module Tests.Route exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route
import Test exposing (..)
import TestUtils exposing (..)
import Url exposing (Url)


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
        , describe "URL parsing"
            [ testString "https://example.com/" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> Expect.equal Nothing
                        ]
            , testString "https://example.com/abc" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> nothing
            , testString "https://example.com/123" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId 123)
                        , .parameters >> .ftsTerm >> Expect.equal Nothing
                        ]
            , testString "https://example.com/123/" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId 123)
                        , .parameters >> .ftsTerm >> Expect.equal Nothing
                        ]
            , testString "https://example.com/?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> Expect.equal (Just "foo")
                        ]
            , testString "https://example.com/123/456?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.TwoIds 123 456)
                        , .parameters >> .ftsTerm >> Expect.equal (Just "foo")
                        ]
            ]
        ]
