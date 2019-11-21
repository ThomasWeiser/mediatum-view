module Query.Filters exposing
    ( alterRoute
    , fromRoute
    , toAttributeTests
    )

import Dict
import Query.Attribute
import Query.Filter as Filter
import Route exposing (Route)
import Types.SearchTerm
import Types.Selection as Selection exposing (Filter(..), Filters)
import Utils


toAttributeTests : Filters -> List Query.Attribute.Test
toAttributeTests filters =
    Selection.filtersToList filters
        |> List.map Filter.toAttributeTest


fromRoute : Route -> Filters
fromRoute route =
    route.parameters.filterByTitle
        |> Types.SearchTerm.setToList
        |> List.map FilterTitleFts
        |> Utils.prependMaybe
            (route.parameters.filterByYear
                |> Maybe.map FilterYearWithin
            )
        |> Selection.filtersFromList


alterRoute : Filters -> Route -> Route
alterRoute filters route =
    let
        listOfFilters =
            Selection.filtersToList filters

        filterByYear =
            listOfFilters
                |> Utils.findMap
                    (\filter ->
                        case filter of
                            FilterYearWithin range ->
                                Just range

                            _ ->
                                Nothing
                    )

        filterByTitle =
            listOfFilters
                |> List.filterMap
                    (\filter ->
                        case filter of
                            FilterTitleFts titleSearchTerm ->
                                Just titleSearchTerm

                            _ ->
                                Nothing
                    )
                |> Types.SearchTerm.setFromList

        parameters =
            route.parameters
    in
    { route
        | parameters =
            { parameters
                | filterByYear = filterByYear
                , filterByTitle = filterByTitle
            }
    }
