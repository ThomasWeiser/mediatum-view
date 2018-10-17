module Query.Filter exposing
    ( Filter(..)
    , toAttributeTest
    )

import Query.Attribute


type Filter
    = YearWithin String String


toAttributeTest : Filter -> Query.Attribute.Test
toAttributeTest filter =
    case filter of
        YearWithin fromYear toYear ->
            { key = "year"
            , operation = Query.Attribute.DateRange fromYear toYear
            }
