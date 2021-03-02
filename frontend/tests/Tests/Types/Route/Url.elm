module Tests.Types.Route.Url exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types.Route
import Types.Aspect as Aspect
import Types.Config exposing (Config)
import Types.FilterList as FilterList
import Types.Id as Id
import Types.Localization as Localization
import Types.Route as Route
import Types.Route.Url
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (Sorting(..))
import Url
import Utils



-- TODO: Add tests and expectations about ftsFilters and facetFilters


testConfig : Config
testConfig =
    { uiLanguage = Localization.LangEn
    , serverConfigAdopted = True
    , defaultPageSize = 10
    , defaultSorting = ByRank
    , numberOfFacetValues = 20
    , ftsAspects = []
    , facetAspects = []
    }


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
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> nothing
            , testString "https://example.com/" <|
                Url.fromString
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .globalFts >> nothing
                        , Types.Route.Url.toString testConfig >> Expect.equal "/"
                        ]
            , testString "https://example.com/123" <|
                Url.fromString
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (Id.fromInt 123))
                        , .parameters >> .globalFts >> nothing
                        , .parameters >> .sorting >> Expect.equal testConfig.defaultSorting
                        , .parameters >> .ftsFilters >> FilterList.isEmpty >> Expect.true "Expecting emtpy set of ftsFilters"
                        , .parameters >> .facetFilters >> FilterList.isEmpty >> Expect.true "Expecting emtpy set of facetFilters"
                        , Types.Route.Url.toString testConfig >> Expect.equal "/123"
                        ]
            , testString "https://example.com/?search=foo" <|
                Url.fromString
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .globalFts >> expectJustSearchTerm "foo"
                        , Types.Route.Url.toString testConfig >> Expect.equal "/?search=foo"
                        ]
            , testString "https://example.com/?sort-by=rank" <|
                Url.fromString
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .globalFts >> nothing
                        , .parameters >> .sorting >> Expect.equal ByRank
                        , Types.Route.Url.toString testConfig
                            >> Expect.equal
                                (testConfig.defaultSorting
                                    == ByRank
                                    |> Utils.ifElse "/" "/?sort-by=rank"
                                )
                        ]
            , testString "https://example.com/?sort-by=date" <|
                Url.fromString
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> justAndThenAll
                        [ .path >> Expect.equal Route.NoId
                        , .parameters >> .globalFts >> nothing
                        , .parameters >> .sorting >> Expect.equal ByDate
                        , Types.Route.Url.toString testConfig
                            >> Expect.equal
                                (testConfig.defaultSorting
                                    == ByDate
                                    |> Utils.ifElse "/" "/?sort-by=date"
                                )
                        ]
            , testString "https://example.com/123/456?search=foo" <|
                Url.fromString
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.TwoIds (Id.fromInt 123) (Id.fromInt 456))
                        , .parameters >> .globalFts >> expectJustSearchTerm "foo"
                        , Types.Route.Url.toString testConfig >> Expect.equal "/123/456?search=foo"
                        ]
            , describe "It should remove search terms that are empty"
                [ testString "https://example.com/?search=&offset=7" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .parameters >> .globalFts >> nothing
                            , .parameters >> .offset >> Expect.equal 7
                            , Types.Route.Url.toString testConfig >> Expect.equal "/?offset=7"
                            ]
                ]
            , describe "It should remove whitespace on both sides of a search term"
                [ testString "https://example.com/789/?search=%20foo%20%20bar%20" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .path >> Expect.equal (Route.OneId (Id.fromInt 789))
                            , .parameters >> .globalFts >> expectJustSearchTerm "foo bar"
                            , Types.Route.Url.toString testConfig >> Expect.equal "/789?search=foo%20bar"
                            ]
                ]
            , describe "It should remove search terms that are only whitespace"
                [ testString "https://example.com/?search=%20%20&offset=7" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .parameters >> .globalFts >> nothing
                            , .parameters >> .offset >> Expect.equal 7
                            ]
                ]
            , describe "It should remove fts filter terms that are empty or only whitespace"
                [ testString "https://example.com/?search-title=&search-person=%20%20&search-author=foo" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .parameters >> .ftsFilters >> FilterList.get (Aspect.fromString "title") >> nothing
                            , .parameters >> .ftsFilters >> FilterList.get (Aspect.fromString "person") >> nothing
                            , .parameters >> .ftsFilters >> FilterList.get (Aspect.fromString "author") >> expectJustSearchTerm "foo"
                            ]
                ]
            , describe "It should keep facet filter values that are empty or contain whitespace"
                [ testString "https://example.com/?has-title=&has-author=%20" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .parameters >> .facetFilters >> FilterList.get (Aspect.fromString "title") >> Expect.equal (Just "")
                            , .parameters >> .facetFilters >> FilterList.get (Aspect.fromString "author") >> Expect.equal (Just " ")
                            ]
                ]
            , describe "Multiple search terms on the same aspect should be concatenated"
                [ testString "https://example.com/?search=f1&search-author=a1&search=f2&search-author=a2" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .parameters >> .globalFts >> expectJustSearchTerm "f1 f2"
                            , .parameters >> .ftsFilters >> FilterList.get (Aspect.fromString "author") >> expectJustSearchTerm "a1 a2"
                            ]
                ]
            , describe "On multiple values of a facet filter on the same aspect: the last value should win"
                [ testString "https://example.com/213?has-title=t1&has-title=t2" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .parameters >> .facetFilters >> FilterList.get (Aspect.fromString "title") >> Expect.equal (Just "t2")
                            ]
                ]
            , testString "https://example.com/789/?search-title=%20foo%20\"bar%20%20baz\"" <|
                Url.fromString
                    >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                    >> justAndThenAll
                        [ .path >> Expect.equal (Route.OneId (Id.fromInt 789))
                        , .parameters >> .globalFts >> nothing
                        , .parameters >> .ftsFilters >> FilterList.get (Aspect.fromString "title") >> expectJustSearchTerm "foo \"bar baz\""
                        , Types.Route.Url.toString testConfig >> Expect.equal "/789?search-title=foo%20%22bar%20baz%22"
                        ]
            , describe "Percent-coding should work also used in a path segment"
                [ testString "https://example.com/45%36" <|
                    Url.fromString
                        >> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        >> justAndThenAll
                            [ .path >> Expect.equal (Route.OneId (Id.fromInt 456))
                            , .parameters >> .globalFts >> nothing
                            ]
                ]
            ]
        , describe "fuzzing routes"
            [ fuzz Tests.Types.Route.fuzzerRoute
                "building and parsing again results in same route"
                (\route ->
                    route
                        |> Types.Route.Url.toString testConfig
                        -- |> Debug.log "toString"
                        |> String.append "https://example.com"
                        |> Url.fromString
                        |> Maybe.andThen (Types.Route.Url.parseUrl testConfig)
                        |> justAndThen (Expect.equal route)
                )
            ]
        ]


expectJustSearchTerm : String -> Maybe SearchTerm -> Expectation
expectJustSearchTerm expectedString =
    justAndThen
        (Types.SearchTerm.toString >> Expect.equal expectedString)
