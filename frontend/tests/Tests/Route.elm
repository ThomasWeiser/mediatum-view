module Tests.Route exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Nonempty exposing (Nonempty)
import Route exposing (Route, RouteFtsSorting(..), RouteParameters, RoutePath(..))
import String.Extra
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
        , describe "URL parsing and building"
            [ testString "https://example.com/abc" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> nothing
            , testString "https://example.com/" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> nothing
                        , Route.toString >> Expect.equal "/"
                        ]
            , testString "https://example.com/123" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId 123)
                        , .parameters >> .ftsTerm >> nothing
                        , Route.toString >> Expect.equal "/123"
                        ]
            , testString "https://example.com/123/" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId 123)
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .ftsSorting >> nothing
                        , .parameters >> .filterByYear >> nothing
                        , .parameters >> .filterByTitle >> nothing
                        , Route.toString >> Expect.equal "/123"
                        ]
            , testString "https://example.com/?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> Expect.equal (Just "foo")
                        , Route.toString >> Expect.equal "/?fts-term=foo"
                        ]
            , testString "https://example.com/123/456?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.TwoIds 123 456)
                        , .parameters >> .ftsTerm >> Expect.equal (Just "foo")
                        , Route.toString >> Expect.equal "/123/456?fts-term=foo"
                        ]
            , describe "It should remove search terms that are empty or only whitespace"
                [ testString "https://example.com/?fts-term=%20%20&filter-by-title=&filter-by-title=%20%20" <|
                    Url.fromString
                        >> Maybe.andThen Route.parseUrl
                        >> justAndThenAll
                            [ .path >> Expect.equal Route.NoId
                            , .parameters >> .ftsTerm >> nothing
                            , .parameters >> .filterByTitle >> nothing
                            , Route.toString >> Expect.equal "/"
                            ]
                ]
            , testString "https://example.com/789/?fts%2Dterm=%20foo%20%20bar%20" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId 789)
                        , .parameters >> .ftsTerm >> Expect.equal (Just "foo bar")
                        , Route.toString >> Expect.equal "/789?fts-term=foo%20bar"
                        ]
            , testString "https://example.com/789/?filter-by-title=%20foo%20&filter-by-year=2001-2011&filter-by-title=\"bar%20%20baz\"&fts-sorting=by-rank" <|
                Url.fromString
                    >> Maybe.andThen Route.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId 789)
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .ftsSorting >> Expect.equal (Just Route.ByRank)
                        , .parameters >> .filterByYear >> Expect.equal (Just ( 2001, 2011 ))
                        , .parameters >> .filterByTitle >> Expect.equal (List.Nonempty.fromList [ "foo", "\"bar baz\"" ])
                        , Route.toString >> Expect.equal "/789?fts-sorting=by-rank&filter-by-year=2001-2011&filter-by-title=foo&filter-by-title=%22bar%20baz%22"
                        ]

            {- I guess percent-coding should work within the path, but it doesn't
               Bug: https://github.com/elm/url/issues/16
                  , testString "https://example.com/a78%39" <|
                      Url.fromString
                          >> Maybe.andThen Route.parseUrl
                          >> justAndThenAll
                              [ .path >> Expect.equal (Route.OneId 789)
                              , .parameters >> .ftsTerm >> nothing
                              ]
            -}
            ]
        , describe "fuzzing routes"
            [ fuzz fuzzerRoute
                "building and parsing again results in same route"
                (\route ->
                    route
                        |> Route.toString
                        -- |> Debug.log "toString"
                        |> String.append "https://example.com"
                        |> Url.fromString
                        |> Maybe.andThen Route.parseUrl
                        |> justAndThen (Expect.equal route)
                )
            ]
        ]


fuzzerRoute : Fuzzer Route
fuzzerRoute =
    Fuzz.map2 Route
        (Fuzz.frequency
            [ ( 10, Fuzz.constant Route.NoId )
            , ( 20, Fuzz.map Route.OneId (Fuzz.intRange 0 999999) )
            , ( 30, Fuzz.map2 Route.TwoIds (Fuzz.intRange 0 999999) (Fuzz.intRange 0 999999) )
            ]
        )
        (Fuzz.map4 RouteParameters
            (fuzzerSearchTerm
                |> Fuzz.maybe
            )
            (Fuzz.oneOf [ Fuzz.constant ByRank, Fuzz.constant ByDate ]
                |> Fuzz.maybe
            )
            (fuzzerYearRange
                |> Fuzz.maybe
            )
            (Fuzz.list fuzzerSearchTerm
                |> Fuzz.map (List.take 4)
                -- Don't use longer lists
                |> Fuzz.map List.Nonempty.fromList
            )
        )


{-| A fuzzer for canonical search terms,
i.e. without whitespace at the left and the right
and without repeated whitespace within the string.

It generates some common search string formats as well as random search strings.

-}
fuzzerSearchTerm : Fuzzer String
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
                (String.Extra.clean >> String.Extra.nonBlank >> Maybe.withDefault "baz")
        ]


{-| A fuzzer for numbers representing years,
favoring years from 1960 to 2040,
but also generating random years between 0 and 99999.
-}
fuzzerYear : Fuzzer Int
fuzzerYear =
    Fuzz.frequency
        [ ( 1, Fuzz.intRange 0 99999 )
        , ( 5, Fuzz.intRange 1960 2040 )
        ]


{-| A fuzzer for pairs of years, favoring canonically sorted pairs.
-}
fuzzerYearRange : Fuzzer ( Int, Int )
fuzzerYearRange =
    Fuzz.andMap
        (Fuzz.tuple
            ( fuzzerYear, fuzzerYear )
        )
        (Fuzz.frequency
            [ ( 1, Fuzz.constant identity )
            , ( 3
              , Fuzz.constant
                    (\( year1, year2 ) ->
                        if year1 <= year2 then
                            ( year1, year2 )

                        else
                            ( year2, year1 )
                    )
              )
            ]
        )
