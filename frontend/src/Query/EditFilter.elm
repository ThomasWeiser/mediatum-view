module Query.EditFilter exposing
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
import Utils


type Return
    = NoReturn
    | Saved Filter
    | Removed
    | Canceled


type alias Model =
    Filter


type Msg
    = Set Filter
    | Submit
    | Cancel


init : Filter -> ( Model, Cmd () )
init filter =
    ( filter
    , Browser.Dom.focus "edit-filter-focus"
        |> Task.attempt (always ())
    )


update : Msg -> Model -> ( Model, Return )
update msg model =
    case msg of
        Set filter ->
            ( filter
            , NoReturn
            )

        Submit ->
            ( model
            , if Filter.isEmpty model then
                Removed

              else
                Saved (Filter.normalize model)
            )

        Cancel ->
            ( model
            , Canceled
            )


view : Model -> Html Msg
view model =
    Html.form [ Html.Events.onSubmit Submit ] <|
        [ Filter.viewEdit
            "edit-filter-focus"
            model
            |> Html.map Set
        , Html.button
            [ Html.Attributes.type_ "submit" ]
            [ Html.text "Ok" ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]
