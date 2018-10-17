module Query.Filter exposing
    ( Filter(..)
    , toAttributeTest
    , view
    )

import Html exposing (Html)
import Query.Attribute


type Filter
    = YearWithin String String


view : Filter -> Html msg
view filter =
    case filter of
        YearWithin fromYear toYear ->
            Html.div []
                [ Html.text "Year within "
                , Html.text fromYear
                , Html.text " and "
                , Html.text toYear
                ]


toAttributeTest : Filter -> Query.Attribute.Test
toAttributeTest filter =
    case filter of
        YearWithin fromYear toYear ->
            { key = "year"
            , operation = Query.Attribute.DateRange fromYear toYear
            }
