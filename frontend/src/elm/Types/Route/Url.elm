module Types.Route.Url exposing
    ( parseUrl
    , toString
    )

{-|

@docs parseUrl
@docs toString

-}

import Erl
import Maybe.Extra
import Regex
import Sort.Dict
import Types.Aspect as Aspect
import Types.Id as Id exposing (NodeId)
import Types.Route as Route exposing (Route, RouteParameters, RoutePath(..))
import Types.SearchTerm as SearchTerm
import Types.Selection exposing (FtsSorting(..))
import Url exposing (Url)
import Url.Builder as Builder
import Utils


parseUrl : Url -> Maybe Route
parseUrl url =
    let
        erl : Erl.Url
        erl =
            Erl.parse (Url.toString url)
    in
    Maybe.map2 Route
        (parsePath erl.path)
        (parseQuery erl.query)


parsePath : List String -> Maybe RoutePath
parsePath segments =
    case List.map idFromString segments of
        [] ->
            Just NoId

        [ Just id1 ] ->
            Just (OneId id1)

        [ Just id1, Just id2 ] ->
            Just (TwoIds id1 id2)

        _ ->
            Nothing


idFromString : String -> Maybe NodeId
idFromString =
    String.toInt
        >> Maybe.map Id.fromInt
        >> Maybe.andThen
            (Utils.ensure Id.isValidId)


parseQuery : List ( String, String ) -> Maybe RouteParameters
parseQuery =
    List.foldl
        (\param ->
            Maybe.andThen (parseQueryParameter param)
        )
        (Just (.parameters Route.initHome))


parseQueryParameter : ( String, String ) -> RouteParameters -> Maybe RouteParameters
parseQueryParameter ( name, value ) routeParameters =
    case name of
        "fts-term" ->
            Just { routeParameters | ftsTerm = SearchTerm.fromString value }

        "fts-sorting" ->
            case value of
                "by-rank" ->
                    Just { routeParameters | ftsSorting = FtsByRank }

                "by-date" ->
                    Just { routeParameters | ftsSorting = FtsByDate }

                _ ->
                    Nothing

        "offset" ->
            String.toInt value
                |> Maybe.map
                    (\intValue ->
                        { routeParameters | offset = intValue }
                    )

        "limit" ->
            String.toInt value
                |> Maybe.map
                    (\intValue ->
                        { routeParameters | limit = intValue }
                    )

        "filter-by-fts" ->
            case Regex.find regexFilter value of
                [ { submatches } ] ->
                    case submatches of
                        [ Just aspectName, maybeAspectValue ] ->
                            case Maybe.andThen SearchTerm.fromString maybeAspectValue of
                                Just searchTerm ->
                                    Just
                                        { routeParameters
                                            | ftsFilters =
                                                Sort.Dict.insert
                                                    (Aspect.fromString aspectName)
                                                    searchTerm
                                                    routeParameters.ftsFilters
                                        }

                                Nothing ->
                                    Just routeParameters

                        _ ->
                            Nothing

                _ ->
                    Nothing

        "filter-by-facet" ->
            case Regex.find regexFilter value of
                [ { submatches } ] ->
                    case submatches of
                        [ Just aspectName, maybeAspectValue ] ->
                            Just
                                { routeParameters
                                    | facetFilters =
                                        Sort.Dict.insert
                                            (Aspect.fromString aspectName)
                                            (maybeAspectValue |> Maybe.withDefault "")
                                            routeParameters.facetFilters
                                }

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            case Regex.find regexHasOrSearchAspect name of
                [ { submatches } ] ->
                    case ( submatches, SearchTerm.fromString value ) of
                        ( [ Just "search", Just aspect ], Just searchTerm ) ->
                            Just
                                { routeParameters
                                    | ftsFilters =
                                        Sort.Dict.insert
                                            (Aspect.fromString aspect)
                                            searchTerm
                                            routeParameters.ftsFilters
                                }

                        ( [ Just "has", Just aspect ], _ ) ->
                            Just
                                { routeParameters
                                    | facetFilters =
                                        Sort.Dict.insert
                                            (Aspect.fromString aspect)
                                            value
                                            routeParameters.facetFilters
                                }

                        _ ->
                            Nothing

                _ ->
                    Just routeParameters


regexFilter : Regex.Regex
regexFilter =
    "^(\\w+):(.*)$"
        |> Regex.fromStringWith { caseInsensitive = False, multiline = True }
        |> Maybe.withDefault Regex.never


regexHasOrSearchAspect : Regex.Regex
regexHasOrSearchAspect =
    "^(has|search)-(\\w+)$"
        |> Regex.fromStringWith { caseInsensitive = False, multiline = True }
        |> Maybe.withDefault Regex.never


{-| -}
toString : Route -> String
toString route =
    Builder.absolute
        (case route.path of
            NoId ->
                []

            OneId id ->
                [ id |> Id.toString ]

            TwoIds id1 id2 ->
                [ id1 |> Id.toString, id2 |> Id.toString ]
        )
        (Maybe.Extra.values
            [ route.parameters.ftsTerm
                |> Maybe.map
                    (SearchTerm.toString
                        >> Builder.string "fts-term"
                    )
            , buildParameterIfNotDefault
                (ftsSortingTostring >> Builder.string "fts-sorting")
                Route.defaultFtsSorting
                route.parameters.ftsSorting
            ]
            ++ List.map
                (\( aspect, searchTerm ) ->
                    Builder.string "filter-by-fts"
                        (Aspect.toString aspect ++ ":" ++ SearchTerm.toString searchTerm)
                )
                (Sort.Dict.toList route.parameters.ftsFilters)
            ++ List.map
                (\( aspect, value ) ->
                    Builder.string "filter-by-facet"
                        (Aspect.toString aspect ++ ":" ++ value)
                )
                (Sort.Dict.toList route.parameters.facetFilters)
            ++ Maybe.Extra.values
                [ buildParameterIfNotDefault
                    (Builder.int "offset")
                    0
                    route.parameters.offset
                , buildParameterIfNotDefault
                    (Builder.int "limit")
                    Route.defaultLimit
                    route.parameters.limit
                ]
        )


ftsSortingTostring : FtsSorting -> String
ftsSortingTostring ftsSorting =
    case ftsSorting of
        FtsByRank ->
            "by-rank"

        FtsByDate ->
            "by-date"


buildParameterIfNotDefault : (a -> Builder.QueryParameter) -> a -> a -> Maybe Builder.QueryParameter
buildParameterIfNotDefault mapParameter defaultValue actualValue =
    if defaultValue == actualValue then
        Nothing

    else
        Just (mapParameter actualValue)
