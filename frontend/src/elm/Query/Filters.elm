module Query.Filters exposing
    ( filtersFromRoute
    , insert
    , none
    , remove
    , toAttributeTests
    , toList
    )

import Data.Types exposing (Filter(..), Filters)
import Data.Types.SearchTerm
import Dict exposing (Dict)
import Query.Attribute
import Query.Filter as Filter
import Route exposing (Route)
import Utils


none : Filters
none =
    Dict.empty


insert : Filter -> Filters -> Filters
insert filter filters =
    Dict.insert (Filter.handle filter) filter filters


remove : String -> Filters -> Filters
remove handle filters =
    Dict.remove handle filters


toList : Filters -> List Filter
toList filters =
    Dict.values filters


toAttributeTests : Filters -> List Query.Attribute.Test
toAttributeTests filters =
    toList filters
        |> List.map Filter.toAttributeTest


filtersFromRoute : Route -> Filters
filtersFromRoute route =
    route.parameters.filterByTitle
        |> Data.Types.SearchTerm.setToList
        |> List.map FilterTitleFts
        |> Utils.prependMaybe
            (route.parameters.filterByYear
                |> Maybe.map FilterYearWithin
            )
        |> List.map (\filter -> ( Filter.handle filter, filter ))
        |> Dict.fromList
