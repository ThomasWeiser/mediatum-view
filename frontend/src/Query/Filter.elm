module Query.Filter exposing
    ( Filter(..)
    , FilterType
    , filterTypes
    , handle
    , isEmpty
    , normalize
    , toAttributeTest
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
    | TitleFts String


type alias FilterType =
    { name : String
    , initFilter : Filter
    }


filterTypes : List FilterType
filterTypes =
    [ { name = "Year", initFilter = initYearWithin }
    , { name = "Title", initFilter = initTitleFts }
    ]


initYearWithin : Filter
initYearWithin =
    YearWithin "" ""


initTitleFts : Filter
initTitleFts =
    TitleFts ""


handle : Filter -> String
handle filter =
    case filter of
        YearWithin fromYear toYear ->
            "YearWithin"

        TitleFts searchTerm ->
            "TitleFts-" ++ searchTerm


normalize : Filter -> Filter
normalize filter =
    case filter of
        YearWithin fromYear toYear ->
            if Maybe.map2 (>) (String.toInt fromYear) (String.toInt toYear) == Just True then
                YearWithin toYear fromYear

            else
                filter

        _ ->
            filter


isEmpty : Filter -> Bool
isEmpty filter =
    case filter of
        YearWithin "" "" ->
            True

        TitleFts "" ->
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

        TitleFts searchTerm ->
            { key = "title"
            , operation = Query.Attribute.SimpleFts searchTerm
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

            TitleFts "" ->
                -- Should never occur here
                [ Html.text "" ]

            TitleFts searchTerm ->
                [ Html.text "Title search: "
                , Html.text searchTerm
                ]


viewEdit : String -> Filter -> Html Filter
viewEdit focusId filter =
    case filter of
        YearWithin from to ->
            Html.span []
                [ Html.input
                    [ Html.Attributes.id focusId
                    , Html.Attributes.type_ "number"
                    , Html.Attributes.min "1900"
                    , Html.Attributes.max "2100"
                    , Html.Attributes.placeholder "from"
                    , Html.Attributes.value from
                    , Utils.onChange (\from1 -> YearWithin from1 to)
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.min "1900"
                    , Html.Attributes.max "2100"
                    , Html.Attributes.placeholder "to"
                    , Html.Attributes.value to
                    , Utils.onChange (\to1 -> YearWithin from to1)
                    ]
                    []
                ]

        TitleFts searchTerm ->
            Html.span []
                [ Html.input
                    [ Html.Attributes.id focusId
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.placeholder "Title full text filter"
                    , Html.Attributes.value searchTerm
                    , Utils.onChange TitleFts
                    ]
                    []
                ]
