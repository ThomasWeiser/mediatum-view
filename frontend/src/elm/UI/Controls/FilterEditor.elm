module UI.Controls.FilterEditor exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , update
    , view
    )

import Browser.Dom
import Filter exposing (Controls(..))
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task
import Time
import Types.Selection exposing (Filter(..))


type Return
    = NoReturn
    | Saved Filter
    | Removed
    | Canceled


type alias Model =
    { controls : Controls
    , focusId : String
    }


type Msg
    = NoOp
    | Change Controls
    | Submit
    | Cancel
    | Focus String Int


init : Controls -> ( Model, Cmd Msg )
init controls =
    ( { controls = controls
      , focusId =
            -- TODO
            "filter-editor-provisional-focus-id"
      }
    , Task.perform
        (\posix ->
            Focus
                (posix |> Time.posixToMillis |> String.fromInt)
                (posix |> Time.toYear Time.utc)
        )
        Time.now
    )


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
            , case Filter.controlsToFilter model.controls of
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


view : Model -> Html Msg
view model =
    Html.form
        [ Html.Events.onSubmit Submit
        , Html.Attributes.class "filter-form input-group"
        ]
        [ Filter.viewEditControls
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
