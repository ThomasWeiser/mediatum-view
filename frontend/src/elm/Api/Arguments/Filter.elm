module Api.Arguments.Filter exposing
    ( ftsFiltersToAspectTests
    , facetFiltersToAspectTests
    )

{-| When using filters in a [`Selection`](Types-Selection) these filter
are translated into certain tests on aspects of the documents.

This module defines that translation for each of the provided filter.

Used internally in module [`Api.Queries`](Api.Queries).

@docs ftsFiltersToAspectTests
@docs facetFiltersToAspectTests

-}

import Api.Arguments.AspectTest
import Types.FilterList as FilterList
import Types.Range as Range
import Types.SearchTerm
import Types.Selection exposing (FacetFilters, FtsFilters)


{-| -}
ftsFiltersToAspectTests : FtsFilters -> List Api.Arguments.AspectTest.Test
ftsFiltersToAspectTests ftsFilters =
    ftsFilters
        |> FilterList.toList
        |> List.map
            (\( aspect, searchTerm ) ->
                { aspect = aspect
                , operation =
                    Api.Arguments.AspectTest.Fts
                        (Types.SearchTerm.toString searchTerm)
                }
            )


{-| -}
facetFiltersToAspectTests : FacetFilters -> List Api.Arguments.AspectTest.Test
facetFiltersToAspectTests facetFilters =
    facetFilters
        |> FilterList.toList
        |> List.map
            (\( aspect, value ) ->
                { aspect = aspect
                , operation = Api.Arguments.AspectTest.Equality value
                }
            )
