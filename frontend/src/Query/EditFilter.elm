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
import Utils


type Return
    = NoReturn
    | Saved Filter
    | Removed
    | Canceled


type alias Model =
    { from : String
    , to : String
    }


type Msg
    = SetFrom String
    | SetTo String
    | Submit
    | Cancel


init : Maybe Filter -> Model
init maybeFilter =
    case maybeFilter of
        Nothing ->
            Model "" ""

        Just (Filter.YearWithin from to) ->
            Model from to


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
            , if model.from == "" && model.to == "" then
                Removed

              else
                Saved <| Filter.YearWithin model.from model.to
            )

        Cancel ->
            ( model
            , Canceled
            )


inputYear : (String -> Msg) -> String -> String -> Html Msg
inputYear onChange placeholder value =
    Html.input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.min "1900"
        , Html.Attributes.max "2100"
        , Html.Attributes.placeholder placeholder
        , Html.Attributes.value value
        , Utils.onChange onChange
        ]
        []


view : Model -> Html Msg
view model =
    Html.form
        [ Html.Events.onSubmit Submit ]
        [ inputYear SetFrom "from" model.from
        , inputYear SetTo "to" model.to
        , Html.button
            [ Html.Attributes.type_ "submit" ]
            [ Html.text "Ok" ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]
