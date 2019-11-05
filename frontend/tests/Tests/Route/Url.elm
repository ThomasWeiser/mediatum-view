module Tests.Route.Url exposing (suite)

import Data.Types exposing (FtsSorting(..), nodeIdFromInt)
import Data.Types.SearchTerm exposing (SearchTerm, SetOfSearchTerms)
import Expect exposing (Expectation)
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Range
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
                        , .parameters >> .ftsTerm >> nothing
                        , Route.Url.toString >> Expect.equal "/"
                        ]
            , testString "https://example.com/123" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (nodeIdFromInt 123))
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .ftsSorting >> Expect.equal Route.defaultFtsSorting
                        , .parameters >> .filterByYear >> nothing
                        , .parameters >> .filterByTitle >> expectEmptySetOfSearchTerms
                        , .parameters >> .filterByTitle >> Expect.equal Data.Types.SearchTerm.emptySet
                        , Route.Url.toString >> Expect.equal "/123"
                        ]
            , testString "https://example.com/?fts-term=foo" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> expectJustSearchTerm "foo"
                        , Route.Url.toString >> Expect.equal "/?fts-term=foo"
                        ]
            , testString "https://example.com/?fts-sorting=by-rank" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .ftsTerm >> nothing
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
                        , .parameters >> .ftsTerm >> nothing
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
                        , .parameters >> .ftsTerm >> expectJustSearchTerm "foo"
                        , Route.Url.toString >> Expect.equal "/123/456?fts-term=foo"
                        ]
            , testString "https://example.com/200/?filter-by-year=2001-2011" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.FromTo 2001 2011))
                        , Route.Url.toString >> Expect.equal "/200?filter-by-year=2001-2011"
                        ]
            , testString "https://example.com/201/?filter-by-year=2011-2001" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.FromTo 2001 2011))
                        , Route.Url.toString >> Expect.equal "/201?filter-by-year=2001-2011"
                        ]
            , testString "https://example.com/202/?filter-by-year=2001-" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.From 2001))
                        , Route.Url.toString >> Expect.equal "/202?filter-by-year=2001-"
                        ]
            , testString "https://example.com/203/?filter-by-year=-2011" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal (Just (Range.To 2011))
                        , Route.Url.toString >> Expect.equal "/203?filter-by-year=-2011"
                        ]
            , testString "https://example.com/204/?filter-by-year=-" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .parameters >> .filterByYear >> Expect.equal Nothing
                        , Route.Url.toString >> Expect.equal "/204"
                        ]
            , describe "It should remove search terms that are empty or only whitespace"
                [ testString "https://example.com/?fts-term=%20%20&filter-by-title=&filter-by-title=%20%20" <|
                    Url.fromString
                        >> Maybe.andThen Route.Url.parseUrl
                        >> justAndThenAll
                            [ .path >> Expect.equal Route.NoId
                            , .parameters >> .ftsTerm >> nothing
                            , .parameters >> .filterByTitle >> expectEmptySetOfSearchTerms
                            , Route.Url.toString >> Expect.equal "/"
                            ]
                ]
            , testString "https://example.com/789/?fts%2Dterm=%20foo%20%20bar%20" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (nodeIdFromInt 789))
                        , .parameters >> .ftsTerm >> expectJustSearchTerm "foo bar"
                        , Route.Url.toString >> Expect.equal "/789?fts-term=foo%20bar"
                        ]
            , testString "https://example.com/789/?filter-by-title=%20foo%20&filter-by-year=2001-2011&filter-by-title=\"bar%20%20baz\"" <|
                Url.fromString
                    >> Maybe.andThen Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (nodeIdFromInt 789))
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .filterByYear >> Expect.equal (Just (Range.FromTo 2001 2011))
                        , .parameters
                            >> .filterByTitle
                            >> expectSetOfSearchTerms [ "\"bar baz\"", "foo" ]
                        , Route.Url.toString >> Expect.equal "/789?filter-by-year=2001-2011&filter-by-title=%22bar%20baz%22&filter-by-title=foo"
                        ]

            {- I guess percent-coding should work within the path, but it doesn't
               Bug: https://github.com/elm/url/issues/16
                  , testString "https://example.com/a78%39" <|
                      Url.fromString
                          >> Maybe.andThen Route.Url.parseUrl
                          >> justAndThenAll
                              [ .path >> Expect.equal (Route.OneId 789)
                              , .parameters >> .ftsTerm >> nothing
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


expectJustSearchTerm : String -> Maybe SearchTerm -> Expectation
expectJustSearchTerm expectedString =
    justAndThen
        (Data.Types.SearchTerm.toString >> Expect.equal expectedString)


expectEmptySetOfSearchTerms : SetOfSearchTerms -> Expectation
expectEmptySetOfSearchTerms =
    Data.Types.SearchTerm.setIsEmpty >> Expect.true "Expected empty set of search terms."


expectSetOfSearchTerms : List String -> SetOfSearchTerms -> Expectation
expectSetOfSearchTerms listOfStrings =
    listOfStrings
        |> List.map Data.Types.SearchTerm.fromString
        |> Maybe.Extra.values
        |> Data.Types.SearchTerm.setFromList
        |> Expect.equal
