module Query.FilterEditor exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , update
    , view
    )

import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Query.Filter as Filter exposing (Filter)
import Task
import Time
import Utils


type Return
    = NoReturn
    | Saved Filter
    | Removed
    | Canceled


type alias Model =
    { filter : Filter
    , focusId : String
    }


type Msg
    = NoOp
    | Change Filter
    | Submit
    | Cancel
    | Focus String


init : Filter -> ( Model, Cmd Msg )
init filter =
    ( { filter = filter
      , focusId = "filter-editor-provisional-focus-id"
      }
    , Task.perform
        (Time.posixToMillis >> String.fromInt >> Focus)
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

        Change filter ->
            ( { model | filter = filter }
            , Cmd.none
            , NoReturn
            )

        Submit ->
            ( model
            , Cmd.none
            , if Filter.isEmpty model.filter then
                Removed

              else
                Saved (Filter.normalize model.filter)
            )

        Cancel ->
            ( model
            , Cmd.none
            , Canceled
            )

        Focus focusId ->
            ( { model | focusId = focusId }
            , Browser.Dom.focus focusId
                |> Task.attempt (always NoOp)
            , NoReturn
            )


view : Model -> Html Msg
view model =
    Html.form [ Html.Events.onSubmit Submit ] <|
        [ Filter.viewEdit
            model.focusId
            model.filter
            |> Html.map Change
        , Html.button
            [ Html.Attributes.type_ "submit" ]
            [ Html.text "Ok" ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]
