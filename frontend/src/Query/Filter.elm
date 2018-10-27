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
    Html.span [] <|
        case filter of
            YearWithin "" "" ->
                -- Should never occur here
                [ Html.text "" ]

            YearWithin fromYear "" ->
                [ Html.text "Years from "
                , Html.text fromYear
                ]

            YearWithin "" toYear ->
                [ Html.text "Years up to "
                , Html.text toYear
                ]

            YearWithin fromYear toYear ->
                if fromYear == toYear then
                    [ Html.text "Year "
                    , Html.text fromYear
                    ]

                else
                    [ Html.text "Years from "
                    , Html.text fromYear
                    , Html.text " to "
                    , Html.text toYear
                    ]
