module Tests.Route exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    describe "URL segmentation and parsing"
        [ describe "URL segmentation"
            [ test "https://example.com:443" <|
                \() ->
                    "https://example.com:443"
                        |> Url.fromString
                        |> Expect.equal
                            (Just
                                { protocol = Url.Https
                                , host = "example.com"
                                , port_ = Just 443
                                , path = "/"
                                , query = Nothing
                                , fragment = Nothing
                                }
                            )
            , test "http://example.com/core/List?q=top%20hat#map" <|
                \() ->
                    "http://example.com/core/List?q=top%20hat#map"
                        |> Url.fromString
                        |> Expect.equal
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
            [ test "https://example.com/" <|
                \() ->
                    "https://example.com/"
                        |> Url.fromString
                        |> Maybe.map Route.parseUrl
                        |> justAndThenAll
                            [ .path >> Expect.equal Route.NoId
                            , .parameters >> .ftsTerm >> Expect.equal Nothing
                            ]
            , test "https://example.com/123" <|
                \() ->
                    "https://example.com/123"
                        |> Url.fromString
                        |> Maybe.map Route.parseUrl
                        |> justAndThenAll
                            [ .path >> Expect.equal (Route.OneId 123)
                            , .parameters >> .ftsTerm >> Expect.equal Nothing
                            ]
            , test "https://example.com/123/" <|
                \() ->
                    "https://example.com/123/"
                        |> Url.fromString
                        |> Maybe.map Route.parseUrl
                        |> justAndThenAll
                            [ .path >> Expect.equal (Route.OneId 123)
                            , .parameters >> .ftsTerm >> Expect.equal Nothing
                            ]
            , test "https://example.com/?fts-term=foo" <|
                \() ->
                    "https://example.com/?fts-term=foo"
                        |> Url.fromString
                        |> Maybe.map Route.parseUrl
                        |> justAndThenAll
                            [ .path >> Expect.equal Route.NoId
                            , .parameters >> .ftsTerm >> Expect.equal (Just "foo")
                            ]
            , test "https://example.com/123/456?fts-term=foo" <|
                \() ->
                    "https://example.com/123/456?fts-term=foo"
                        |> Url.fromString
                        |> Maybe.map Route.parseUrl
                        |> justAndThenAll
                            [ .path >> Expect.equal (Route.TwoIds 123 456)
                            , .parameters >> .ftsTerm >> Expect.equal (Just "foo")
                            ]
            ]
        ]


justAndThen : (subject -> Expectation) -> Maybe subject -> Expectation
justAndThen thunk maybeSubject =
    case maybeSubject of
        Nothing ->
            Expect.fail "Nothing\n╷\n│ expect (Just ...)"

        Just value ->
            thunk value


justAndThenAll : List (subject -> Expectation) -> Maybe subject -> Expectation
justAndThenAll thunks maybeSubject =
    case maybeSubject of
        Nothing ->
            Expect.fail "Nothing\n╷\n│ expect (Just ...)"

        Just value ->
            Expect.all thunks value


just : Maybe subject -> Expectation
just maybeSubject =
    case maybeSubject of
        Nothing ->
            Expect.fail "Nothing\n╷\n│ expect (Just _)"

        Just value ->
            Expect.pass


nothing : Maybe subject -> Expectation
nothing maybeSubject =
    Expect.equal Nothing maybeSubject
