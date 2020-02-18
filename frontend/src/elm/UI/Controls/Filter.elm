module UI.Controls.Filter exposing
    ( FilterType
    , Controls(..)
    , filterTypes
    , controlsFromFilter
    , controlsToFilter
    , viewFilterDescription
    , viewEditControls
    )

{-|

@docs FilterType
@docs Controls
@docs filterTypes
@docs controlsFromFilter
@docs controlsToFilter
@docs viewFilterDescription
@docs viewEditControls

-}

import Basics.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import Types.Range as Range
import Types.SearchTerm
import Types.Selection exposing (Filter(..))
import Utils


{-| -}
type alias FilterType =
    { name : String
    , initControls : Controls
    }


{-| -}
type Controls
    = ControlsYearWithin (Maybe Int) (Maybe Int)
    | ControlsTitleFts String


{-| -}
filterTypes : List FilterType
filterTypes =
    [ { name = "Year"
      , initControls = ControlsYearWithin Nothing Nothing
      }
    , { name = "Title"
      , initControls = ControlsTitleFts ""
      }
    ]


{-| -}
controlsFromFilter : Filter -> Controls
controlsFromFilter filter =
    case filter of
        FilterYearWithin range ->
            Basics.Extra.uncurry ControlsYearWithin
                (Range.toMaybe range)

        FilterTitleFts searchTerm ->
            ControlsTitleFts (Types.SearchTerm.toString searchTerm)


{-| -}
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


{-| -}
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


{-| -}
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
                    , Html.Attributes.placeholder "From Year"
                    , Html.Attributes.value
                        (Maybe.Extra.unwrap "" String.fromInt from)
                    , Html.Events.onInput
                        (\from1 ->
                            ControlsYearWithin (String.toInt from1) to
                        )
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.min "1900"
                    , Html.Attributes.max "2100"
                    , Html.Attributes.placeholder "To Year"
                    , Html.Attributes.value
                        (Maybe.Extra.unwrap "" String.fromInt to)
                    , Html.Events.onInput
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
                    , Html.Attributes.placeholder "Title Filter"
                    , Html.Attributes.value searchTerm
                    , Html.Events.onInput ControlsTitleFts
                    ]
                    []
                ]
