module UI.Controls.FilterEditor exposing
    ( Return(..)
    , Model
    , Msg
    , init
    , update
    , view
    )

{-|

@docs Return
@docs Model
@docs Msg
@docs init
@docs update
@docs view

-}

import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task
import Time
import Types.Selection exposing (Filter(..))
import UI.Controls.Filter exposing (Controls(..))


{-| -}
type Return
    = NoReturn
    | Saved Filter
    | Removed
    | Canceled


{-| -}
type alias Model =
    { controls : Controls
    , focusId : String
    }


{-| -}
type Msg
    = NoOp
    | Change Controls
    | Submit
    | Cancel
    | Focus String Int


{-| -}
init : Controls -> ( Model, Cmd Msg )
init controls =
    ( { controls = controls
      , focusId = ""
      }
    , Task.perform
        (\posix ->
            Focus
                (posix |> Time.posixToMillis |> String.fromInt)
                (posix |> Time.toYear Time.utc)
        )
        Time.now
    )


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , NoReturn
            )

        Change controls ->
            ( { model | controls = controls }
            , Cmd.none
            , NoReturn
            )

        Submit ->
            ( model
            , Cmd.none
            , case UI.Controls.Filter.controlsToFilter model.controls of
                Nothing ->
                    Removed

                Just filter ->
                    Saved filter
            )

        Cancel ->
            ( model
            , Cmd.none
            , Canceled
            )

        Focus focusId year ->
            ( { model
                | focusId = focusId
                , controls =
                    case model.controls of
                        ControlsYearWithin Nothing Nothing ->
                            ControlsYearWithin
                                (Just (year - 9))
                                (Just year)

                        _ ->
                            model.controls
              }
            , Browser.Dom.focus focusId
                |> Task.attempt (always NoOp)
            , NoReturn
            )


{-| -}
view : Model -> Html Msg
view model =
    Html.form
        [ Html.Events.onSubmit Submit
        , Html.Attributes.class "filter-form input-group"
        ]
        [ UI.Controls.Filter.viewEditControls
            model.focusId
            model.controls
            |> Html.map Change
        , Html.button
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.class "filter-button"
            ]
            [ Html.text "Ok" ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick Cancel
            , Html.Attributes.class "filter-button"
            ]
            [ Html.text "Cancel" ]
        ]
