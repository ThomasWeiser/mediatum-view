module Article.Empty exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Html exposing (Html)


type alias Model =
    ()


type Msg
    = NoOp


initialModel : Model
initialModel =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text ""
