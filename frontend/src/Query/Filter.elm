module Query.Filter exposing
    ( Filter(..)
    , Msg(..)
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


type Msg
    = Remove
    | Edit


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


view : Filter -> Html Msg
view filter =
    case filter of
        YearWithin fromYear toYear ->
            Html.div []
                [ Html.text "Year within "
                , Html.text fromYear
                , Html.text " and "
                , Html.text toYear
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick Edit
                    ]
                    [ Html.text "Edit" ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick Remove
                    ]
                    [ Html.text "Remove" ]
                ]
