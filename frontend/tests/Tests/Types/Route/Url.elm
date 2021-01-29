module Tests.Types.Route.Url exposing (suite)

import Expect exposing (Expectation)
import Sort.Dict
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types.Route
import Types.Aspect as Aspect exposing (Aspect)
import Types.Id as Id
import Types.Range as Range
import Types.Route as Route
import Types.Route.Url
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (FtsSorting(..))
import Url
import Utils



-- TODO: Add tests and expectations about ftsFilters and facetFilters


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
                        , .parameters >> .ftsFilters >> Sort.Dict.isEmpty >> Expect.true "Expecting emtpy set of ftsFilters"
                        , .parameters >> .facetFilters >> Sort.Dict.isEmpty >> Expect.true "Expecting emtpy set of facetFilters"
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
            , testString "https://example.com/?sort-by=rank" <|
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
                                    |> Utils.ifElse "/" "/?sort-by=rank"
                                )
                        ]
            , testString "https://example.com/?sort-by=date" <|
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
                                    |> Utils.ifElse "/" "/?sort-by=date"
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
            , describe "It should remove search terms that are empty"
                [ testString "https://example.com/?fts-term=&offset=7" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .parameters >> .ftsTerm >> nothing
                            , .parameters >> .offset >> Expect.equal 7
                            , Types.Route.Url.toString >> Expect.equal "/?offset=7"
                            ]
                ]
            , describe "It should remove whitespace on both sides of a search term"
                [ testString "https://example.com/789/?fts%2Dterm=%20foo%20%20bar%20" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .path >> Expect.equal (Route.OneId (Id.fromInt 789))
                            , .parameters >> .ftsTerm >> expectJustSearchTerm "foo bar"
                            , Types.Route.Url.toString >> Expect.equal "/789?fts-term=foo%20bar"
                            ]
                ]
            , describe "It should remove search terms that are only whitespace"
                [ testString "https://example.com/?fts-term=%20%20&offset=7" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .parameters >> .ftsTerm >> nothing
                            , .parameters >> .offset >> Expect.equal 7
                            ]
                ]
            , describe "It should remove fts filter terms that are empty or only whitespace"
                [ testString "https://example.com/?search-title=&search-person=%20%20&search-author=foo" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .parameters >> .ftsFilters >> Sort.Dict.get (Aspect.fromString "title") >> nothing
                            , .parameters >> .ftsFilters >> Sort.Dict.get (Aspect.fromString "person") >> nothing
                            , .parameters >> .ftsFilters >> Sort.Dict.get (Aspect.fromString "author") >> expectJustSearchTerm "foo"
                            ]
                ]
            , describe "It should keep facet filter values that are empty or contain whitespace"
                [ testString "https://example.com/?has-title=&has-author=%20" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .parameters >> .facetFilters >> Sort.Dict.get (Aspect.fromString "title") >> Expect.equal (Just "")
                            , .parameters >> .facetFilters >> Sort.Dict.get (Aspect.fromString "author") >> Expect.equal (Just " ")
                            ]
                ]
            , describe "Multiple search terms on the same aspect should be concatenated"
                [ testString "https://example.com/?fts-term=f1&search-author=a1&fts-term=f2&search-author=a2" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .parameters >> .ftsTerm >> expectJustSearchTerm "f1 f2"
                            , .parameters >> .ftsFilters >> Sort.Dict.get (Aspect.fromString "author") >> expectJustSearchTerm "a1 a2"
                            ]
                ]
            , describe "On multiple values of a facet filter on the same aspect: the last value should win"
                [ testString "https://example.com/213?has-title=t1&has-title=t2" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .parameters >> .facetFilters >> Sort.Dict.get (Aspect.fromString "title") >> Expect.equal (Just "t2")
                            ]
                ]
            , testString "https://example.com/789/?search-title=%20foo%20\"bar%20%20baz\"" <|
                Url.fromString
                    >> Maybe.andThen Types.Route.Url.parseUrl
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (Id.fromInt 789))
                        , .parameters >> .ftsTerm >> nothing
                        , .parameters >> .ftsFilters >> Sort.Dict.get (Aspect.fromString "title") >> expectJustSearchTerm "foo \"bar baz\""
                        , Types.Route.Url.toString >> Expect.equal "/789?search-title=foo%20%22bar%20baz%22"
                        ]
            , describe "Percent-coding should work also used in a path segment"
                [ testString "https://example.com/45%36" <|
                    Url.fromString
                        >> Maybe.andThen Types.Route.Url.parseUrl
                        >> justAndThenAll
                            [ .path >> Expect.equal (Route.OneId (Id.fromInt 456))
                            , .parameters >> .ftsTerm >> nothing
                            ]
                ]
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
