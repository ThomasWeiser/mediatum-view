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
    | Focus String Int


init : Filter -> ( Model, Cmd Msg )
init filter =
    ( { filter = filter
      , focusId = "filter-editor-provisional-focus-id"
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

        Focus focusId year ->
            ( { model
                | focusId = focusId
                , filter =
                    case model.filter of
                        Filter.YearWithin "" "" ->
                            Filter.YearWithin
                                (String.fromInt (year - 9))
                                (String.fromInt year)

                        _ ->
                            model.filter
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
        [ Filter.viewEdit
            model.focusId
            model.filter
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
