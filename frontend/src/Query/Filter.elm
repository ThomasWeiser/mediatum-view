module Query.Filter exposing
    ( Filter(..)
    , initYearWithin
    , isEmpty
    , normalize
    , toAttributeTest
    , toString
    , view
    , viewEdit
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Query.Attribute
import Utils


type Filter
    = YearWithin String String


initYearWithin : Filter
initYearWithin =
    YearWithin "" ""


toString : Filter -> String
toString filter =
    case filter of
        YearWithin fromYear toYear ->
            "YearWithin-" ++ fromYear ++ "-" ++ toYear


normalize : Filter -> Filter
normalize filter =
    case filter of
        YearWithin fromYear toYear ->
            if Maybe.map2 (>) (String.toInt fromYear) (String.toInt toYear) == Just True then
                YearWithin toYear fromYear

            else
                filter


isEmpty : Filter -> Bool
isEmpty filter =
    case filter of
        YearWithin "" "" ->
            True

        _ ->
            False


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


viewEdit : Filter -> Html Filter
viewEdit filter =
    case filter of
        YearWithin from to ->
            Html.span []
                [ inputYear "from" from
                    |> Html.map (\from1 -> YearWithin from1 to)
                , inputYear "to" to
                    |> Html.map (\to1 -> YearWithin from to1)
                ]


inputYear : String -> String -> Html String
inputYear placeholder value =
    Html.input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.min "1900"
        , Html.Attributes.max "2100"
        , Html.Attributes.placeholder placeholder
        , Html.Attributes.value value
        , Utils.onChange identity
        ]
        []
