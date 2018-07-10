module Article.Collection
    exposing
        ( Model
        , Context
        , Msg
        , init
        , update
        , view
        )

import Html exposing (Html)
import Folder exposing (Folder, FolderId)


type alias Context =
    { folder : Folder
    }


type alias Model =
    ()


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Context -> Model -> Html Msg
view context model =
    Html.div []
        [ Html.h3 [] <|
            if Folder.isRoot context.folder then
                [ Html.text "Front page for root of all collections"
                ]
            else
                [ Html.text "Front page for collection \""
                , Html.text context.folder.name
                , Html.text "\""
                ]
        ]
