module Query.Filter exposing
    ( Filter(..)
    , Msg(..)
    , toAttributeTest
    , view
    )

import Html exposing (Html)
import Html.Events
import Query.Attribute


type Filter
    = YearWithin String String


type Msg
    = Remove


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
                    [ Html.Events.onClick Remove ]
                    [ Html.text "Remove" ]
                ]


toAttributeTest : Filter -> Query.Attribute.Test
toAttributeTest filter =
    case filter of
        YearWithin fromYear toYear ->
            { key = "year"
            , operation = Query.Attribute.DateRange fromYear toYear
            }
