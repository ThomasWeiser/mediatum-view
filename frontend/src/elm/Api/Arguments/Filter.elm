module Api.Arguments.Filter exposing
    ( filtersToAttributeTests
    , facetFiltersToAttributeTests
    )

{-| When using filters in a [`Selection`](Types-Selection) these filter
are translated into certain tests on attributes of the documents.

This module defines that translation for each of the provided filter.

Used internally in module [`Api.Queries`](Api.Queries).

@docs filtersToAttributeTests
@docs facetFiltersToAttributeTests

-}

import Api.Arguments.AttributeTest
import Dict
import Types.Range as Range
import Types.SearchTerm
import Types.Selection exposing (FacetFilters, Filter(..), SetOfFilters)


{-| -}
filtersToAttributeTests : SetOfFilters -> List Api.Arguments.AttributeTest.Test
filtersToAttributeTests filters =
    Types.Selection.filtersToList filters
        |> List.map filterToAttributeTest


{-| -}
filterToAttributeTest : Filter -> Api.Arguments.AttributeTest.Test
filterToAttributeTest filter =
    case filter of
        FilterYearWithin range ->
            { key = "year"
            , operation =
                Api.Arguments.AttributeTest.DateRange
                    (Range.unwrap "" String.fromInt range)
            }

        FilterTitleFts searchTerm ->
            { key = "title"
            , operation =
                Api.Arguments.AttributeTest.SimpleFts
                    (Types.SearchTerm.toString searchTerm)
            }


{-| -}
facetFiltersToAttributeTests : FacetFilters -> List Api.Arguments.AttributeTest.Test
facetFiltersToAttributeTests facetFilters =
    facetFilters
        |> Dict.toList
        |> List.map
            (\( key, value ) ->
                { key = key
                , operation = Api.Arguments.AttributeTest.EqualityWithBlankNull value
                }
            )
