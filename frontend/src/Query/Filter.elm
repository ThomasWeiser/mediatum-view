module Query.Filter exposing
    ( Filter(..)
    , toAttributeTest
    , toString
    , view
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Query.Attribute


type Filter
    = YearWithin String String


toString : Filter -> String
toString filter =
    case filter of
        YearWithin fromYear toYear ->
            "YearWithin-" ++ fromYear ++ "-" ++ toYear


toAttributeTest : Filter -> Query.Attribute.Test
toAttributeTest filter =
    case filter of
        YearWithin fromYear toYear ->
            { key = "year"
            , operation = Query.Attribute.DateRange fromYear toYear
            }


view : Filter -> Html Never
view filter =
    case filter of
        YearWithin fromYear toYear ->
            Html.span []
                [ Html.text "Year within "
                , Html.text fromYear
                , Html.text " and "
                , Html.text toYear
                ]
