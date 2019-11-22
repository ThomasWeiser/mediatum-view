module UI.Controls.Filter exposing
    ( Controls(..)
    , FilterType
    , controlsFromFilter
    , controlsToFilter
    , filterTypes
    , viewEditControls
    , viewFilterDescription
    )

import Api.Arguments.AttributeTest
import Basics.Extra
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Range
import Route exposing (Route)
import Types.SearchTerm
import Types.Selection as Selection exposing (Filter(..), SetOfFilters)
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
    [ { name = "Year"
      , initControls = ControlsYearWithin Nothing Nothing
      }
    , { name = "Title"
      , initControls = ControlsTitleFts ""
      }
    ]


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


viewFilterDescription : Filter -> List (Html msg)
viewFilterDescription filter =
    let
        quote text =
            Html.i [] [ Html.text text ]
    in
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


viewEditControls : String -> Controls -> Html Controls
viewEditControls focusId controls =
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
