module Types.Route.Filter exposing
    ( alterRoute
    , fromRoute
    )

{-| Conversion functions between a `SetOfFilters` and the filter parameters of a `Route`.

@docs alterRoute
@docs fromRoute

-}

import Maybe.Extra
import Types.Route exposing (Route)
import Types.Selection as Selection exposing (Filter(..), SetOfFilters)
import Utils


{-| -}
fromRoute : Route -> SetOfFilters
fromRoute route =
    [ route.parameters.filterByYear |> Maybe.map FilterYearWithin
    , route.parameters.filterByTitle |> Maybe.map FilterTitleFts
    ]
        |> Maybe.Extra.values
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
                |> Utils.findMap
                    (\filter ->
                        case filter of
                            FilterTitleFts titleSearchTerm ->
                                Just titleSearchTerm

                            _ ->
                                Nothing
                    )

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
