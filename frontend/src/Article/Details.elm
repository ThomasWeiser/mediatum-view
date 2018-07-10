module Article.Details
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        )

import Html exposing (Html)
import Folder exposing (Folder, FolderId)


type alias Model =
    { id : Int
    }


type Msg
    = NoOp


init : Int -> ( Model, Cmd Msg )
init id =
    ( { id = id }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h3 [] <|
    
                [ Html.text "Display for document "
                , Html.text (toString model.id)
                ]
        ]
