module Types.Route.Filter exposing
    ( alterRoute
    , fromRoute
    )

{-|

@docs alterRoute
@docs fromRoute

-}

import Types.Route exposing (Route)
import Types.SearchTerm
import Types.Selection as Selection exposing (Filter(..), SetOfFilters)
import Utils


{-| -}
fromRoute : Route -> SetOfFilters
fromRoute route =
    route.parameters.filterByTitle
        |> Types.SearchTerm.setToList
        |> List.map FilterTitleFts
        |> Utils.prependMaybe
            (route.parameters.filterByYear
                |> Maybe.map FilterYearWithin
            )
        |> Selection.filtersFromList


{-| -}
alterRoute : SetOfFilters -> Route -> Route
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
