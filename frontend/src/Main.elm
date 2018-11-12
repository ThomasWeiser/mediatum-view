module Main exposing (main)

import App
import Browser
import Browser.Navigation
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Route
import Url exposing (Url)


type alias Model =
    { navigationKey : Browser.Navigation.Key
    , app : App.Model
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , onUrlRequest = UrlRequest << Debug.log "onUrlRequest"
        , onUrlChange = UrlChanged << Debug.log "onUrlChange"
        }


type Msg
    = NoOp
    | UrlRequest Browser.UrlRequest
    | UrlChanged Url.Url
    | AppMsg App.Msg


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    let
        ( appModel, appCmd ) =
            App.init (Route.parseUrl url)
    in
    ( { navigationKey = navigationKey
      , app = appModel
      }
    , Cmd.map AppMsg appCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl
                        model.navigationKey
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        UrlChanged url ->
            let
                route =
                    Route.parseUrl url

                ( subModel, subCmd ) =
                    App.changeRouteTo route model.app
            in
            ( { model
                | app = subModel
              }
            , Cmd.map AppMsg subCmd
            )

        AppMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    App.update subMsg model.app
            in
            ( { model
                | app = subModel
              }
            , Cmd.batch
                [ Cmd.map AppMsg subCmd
                , case subReturn of
                    App.ReflectRoute route ->
                        Browser.Navigation.pushUrl
                            model.navigationKey
                            (Route.toString route)

                    App.NoReturn ->
                        Cmd.none
                ]
            )


view : Model -> Browser.Document Msg
view model =
    { title = "mediaTUM View"
    , body =
        [ App.view model.app
            |> Html.map AppMsg
        ]
    }
