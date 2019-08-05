module Article.Collection exposing
    ( Context
    , Model
    , Msg
    , init
    , update
    , view
    )

import Folder exposing (Folder)
import Html exposing (Html)
import Query


type alias Context =
    { folderQuery : Query.FolderQuery
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
            if Folder.isRoot context.folderQuery.folder then
                [ Html.text "Front page for root of all collections"
                ]

            else
                [ Html.text "Front page for collection \""
                , Html.text context.folderQuery.folder.name
                , Html.text "\""
                ]
        ]
