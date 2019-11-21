module Query.Filter exposing
    ( Controls(..)
    , FilterType
    , controlsFromFilter
    , controlsToFilter
    , filterTypes
    , toAttributeTest
    , view
    , viewEdit
    )

import Basics.Extra
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Query.Attribute
import Range
import Types.SearchTerm
import Types.Selection exposing (Filter(..))
import Utils


type alias FilterType =
    { name : String
    , initControls : Controls
    }


type Controls
    = ControlsYearWithin (Maybe Int) (Maybe Int)
    | ControlsTitleFts String


filterTypes : List FilterType
filterTypes =
    [ { name = "Year", initControls = initControlsYearWithin }
    , { name = "Title", initControls = initControlsTitleFts }
    ]


initControlsYearWithin : Controls
initControlsYearWithin =
    ControlsYearWithin Nothing Nothing


initControlsTitleFts : Controls
initControlsTitleFts =
    ControlsTitleFts ""


controlsFromFilter : Filter -> Controls
controlsFromFilter filter =
    case filter of
        FilterYearWithin range ->
            Basics.Extra.uncurry ControlsYearWithin
                (Range.toMaybe range)

        FilterTitleFts searchTerm ->
            ControlsTitleFts (Types.SearchTerm.toString searchTerm)


controlsToFilter : Controls -> Maybe Filter
controlsToFilter controls =
    case controls of
        ControlsYearWithin from to ->
            Range.fromMaybe ( from, to )
                |> Maybe.map FilterYearWithin

        ControlsTitleFts searchTerm ->
            searchTerm
                |> Types.SearchTerm.fromString
                |> Maybe.map FilterTitleFts


toAttributeTest : Filter -> Query.Attribute.Test
toAttributeTest filter =
    case filter of
        FilterYearWithin range ->
            { key = "year"
            , operation =
                Query.Attribute.DateRange
                    (Range.unwrap "" String.fromInt range)
            }

        FilterTitleFts searchTerm ->
            { key = "title"
            , operation =
                Query.Attribute.SimpleFts
                    (Types.SearchTerm.toString searchTerm)
            }


view : Filter -> List (Html msg)
view filter =
    case filter of
        FilterYearWithin (Range.From fromYear) ->
            [ Html.text "Years from "
            , quote (String.fromInt fromYear)
            ]

        FilterYearWithin (Range.To toYear) ->
            [ Html.text "Years up to "
            , quote (String.fromInt toYear)
            ]

        FilterYearWithin (Range.FromTo fromYear toYear) ->
            if fromYear == toYear then
                [ Html.text "Year "
                , quote (String.fromInt fromYear)
                ]

            else
                [ Html.text "Years from "
                , quote (String.fromInt fromYear)
                , Html.text " to "
                , quote (String.fromInt toYear)
                ]

        FilterTitleFts searchTerm ->
            [ Html.text "Title: "
            , quote (Types.SearchTerm.toString searchTerm)
            ]


viewEdit : String -> Controls -> Html Controls
viewEdit focusId controls =
    case controls of
        ControlsYearWithin from to ->
            Html.span
                [ Html.Attributes.class "filter-inputs" ]
                [ Html.input
                    [ Html.Attributes.id focusId
                    , Html.Attributes.type_ "number"
                    , Html.Attributes.min "1900"
                    , Html.Attributes.max "2100"
                    , Html.Attributes.placeholder "from"
                    , Html.Attributes.value
                        (Maybe.Extra.unwrap "" String.fromInt from)
                    , Utils.onChange
                        (\from1 ->
                            ControlsYearWithin (String.toInt from1) to
                        )
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.min "1900"
                    , Html.Attributes.max "2100"
                    , Html.Attributes.placeholder "to"
                    , Html.Attributes.value
                        (Maybe.Extra.unwrap "" String.fromInt to)
                    , Utils.onChange
                        (\to1 ->
                            ControlsYearWithin from (String.toInt to1)
                        )
                    ]
                    []
                ]

        ControlsTitleFts searchTerm ->
            Html.span
                [ Html.Attributes.class "filter-inputs" ]
                [ Html.input
                    [ Html.Attributes.id focusId
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.placeholder "Title full text filter"
                    , Html.Attributes.value searchTerm
                    , Utils.onChange ControlsTitleFts
                    ]
                    []
                ]


quote : String -> Html msg
quote text =
    Html.i [] [ Html.text text ]
