module Query.EditFilter exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Query.Filter as Filter exposing (Filter)


type Return
    = NoReturn
    | NewFilter Filter


type alias Model =
    { from : String
    , to : String
    }


type Msg
    = SetFrom String
    | SetTo String
    | Submit


init : Model
init =
    Model "" ""


update : Msg -> Model -> ( Model, Return )
update msg model =
    case msg of
        SetFrom input ->
            ( { model | from = input }
            , NoReturn
            )

        SetTo input ->
            ( { model | to = input }
            , NoReturn
            )

        Submit ->
            ( model
            , NewFilter <| Filter.YearWithin model.from model.to
            )


view : Model -> Html Msg
view model =
    Html.form
        [ Html.Events.onSubmit Submit ]
        [ Html.input
            [ Html.Attributes.type_ "input"
            , Html.Attributes.placeholder "from year"
            , Html.Attributes.value model.from
            , Html.Events.onInput SetFrom
            ]
            []
        , Html.input
            [ Html.Attributes.type_ "input"
            , Html.Attributes.placeholder "to year"
            , Html.Attributes.value model.to
            , Html.Events.onInput SetTo
            ]
            []
        , Html.button
            [ Html.Attributes.type_ "submit"

            -- , Html.Attributes.value "Ok" -- TODO ??? (Auch in Controls.elm)
            ]
            [ Html.text "Ok" ]
        ]
