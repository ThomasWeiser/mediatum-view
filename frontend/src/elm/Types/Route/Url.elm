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
import Types.Aspect as Aspect
import Types.Config exposing (Config)
import Types.FilterList as FilterList
import Types.Id as Id exposing (NodeId)
import Types.Route as Route exposing (Route, RouteParameters, RoutePath(..))
import Types.SearchTerm as SearchTerm
import Types.Selection exposing (Sorting(..))
import Url exposing (Url)
import Url.Builder as Builder
import Utils


{-| -}
parseUrl : Config -> Url -> Maybe Route
parseUrl config url =
    let
        erl : Erl.Url
        erl =
            Erl.parse (Url.toString url)
    in
    Maybe.map2 Route
        (parsePath erl.path)
        (parseQuery erl.query (Route.initHome config |> .parameters))


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


parseQuery : List ( String, String ) -> RouteParameters -> Maybe RouteParameters
parseQuery listOfParameters initRouteParameters =
    List.foldl
        (\param ->
            Maybe.andThen (parseQueryParameter param)
        )
        (Just initRouteParameters)
        listOfParameters


parseQueryParameter : ( String, String ) -> RouteParameters -> Maybe RouteParameters
parseQueryParameter ( name, value ) routeParameters =
    case name of
        "search" ->
            Just
                { routeParameters
                    | globalFts =
                        SearchTerm.concatMaybes
                            routeParameters.globalFts
                            (SearchTerm.fromString value)
                }

        "sort-by" ->
            case value of
                "rank" ->
                    Just { routeParameters | sorting = ByRank }

                "date" ->
                    Just { routeParameters | sorting = ByDate }

                _ ->
                    Nothing

        "limit" ->
            String.toInt value
                |> Maybe.map
                    (\intValue ->
                        { routeParameters | limit = intValue }
                    )

        _ ->
            -- Parse parameters like "search-title=foo" or "has-author=bar"
            case Regex.find regexHasOrSearchAspect name of
                [ { submatches } ] ->
                    case submatches of
                        [ Just "search", Just aspect ] ->
                            Just
                                { routeParameters
                                    | ftsFilters =
                                        -- Multiple search terms on the same aspect are concatenated
                                        FilterList.update
                                            (Aspect.fromString aspect)
                                            (\maybeExistingSearchTerm ->
                                                SearchTerm.concatMaybes
                                                    maybeExistingSearchTerm
                                                    (SearchTerm.fromString value)
                                            )
                                            routeParameters.ftsFilters
                                }

                        [ Just "has", Just aspect ] ->
                            Just
                                { routeParameters
                                    | facetFilters =
                                        -- Multiple facet values on the same aspect: last value wins
                                        FilterList.update
                                            (Aspect.fromString aspect)
                                            (always (Just value))
                                            routeParameters.facetFilters
                                }

                        _ ->
                            Nothing

                _ ->
                    -- TODO Shall we really ignore all parameters with unknown keys?
                    Just routeParameters


regexHasOrSearchAspect : Regex.Regex
regexHasOrSearchAspect =
    "^(has|search)-(\\w+)$"
        |> Regex.fromStringWith { caseInsensitive = False, multiline = True }
        |> Maybe.withDefault Regex.never


{-| -}
toString : Config -> Route -> String
toString config route =
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
            [ route.parameters.globalFts
                |> Maybe.map
                    (SearchTerm.toString
                        >> Builder.string "search"
                    )
            , buildParameterIfNotDefault
                (sortingTostring >> Builder.string "sort-by")
                config.defaultSorting
                route.parameters.sorting
            ]
            ++ List.map
                (\( aspect, searchTerm ) ->
                    Builder.string
                        ("search-" ++ Aspect.toString aspect)
                        (SearchTerm.toString searchTerm)
                )
                (FilterList.toList route.parameters.ftsFilters)
            ++ List.map
                (\( aspect, value ) ->
                    Builder.string
                        ("has-" ++ Aspect.toString aspect)
                        value
                )
                (FilterList.toList route.parameters.facetFilters)
            ++ Maybe.Extra.values
                [ buildParameterIfNotDefault
                    (Builder.int "limit")
                    config.defaultPageSize
                    route.parameters.limit
                ]
        )


sortingTostring : Sorting -> String
sortingTostring ftsSorting =
    case ftsSorting of
        ByRank ->
            "rank"

        ByDate ->
            "date"


buildParameterIfNotDefault : (a -> Builder.QueryParameter) -> a -> a -> Maybe Builder.QueryParameter
buildParameterIfNotDefault mapParameter defaultValue actualValue =
    if defaultValue == actualValue then
        Nothing

    else
        Just (mapParameter actualValue)
