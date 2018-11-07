module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Page
import Url exposing (Url)


type alias Model =
    { page : Page.Model
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , onUrlRequest = always NoOp << Debug.log "onUrlRequest"
        , onUrlChange = always NoOp << Debug.log "onUrlChange"
        }


type Msg
    = NoOp
    | PageMsg Page.Msg


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    let
        ( pageModel, pageCmd ) =
            Page.init ()
    in
    ( { page = pageModel }
    , Cmd.map PageMsg pageCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PageMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Page.update subMsg model.page
            in
            ( { model
                | page = subModel
              }
            , Cmd.map PageMsg subCmd
            )


view : Model -> Browser.Document Msg
view model =
    { title = "mediaTUM View"
    , body =
        [ Page.view model.page
            |> Html.map PageMsg
        ]
    }
