module Main exposing (main)

{-| The `Main` module is responsible for:

  - Initializing the app as a [`Browser.application`](/packages/elm/browser/1.0.1/Browser#application) that takes full control over the browser page.

  - Hosting the module [`App`](App).

  - Mananging URL changes by means of:
      - [`onUrlRequest`](/packages/elm/browser/1.0.1/Browser#application) (when the user clicks a link)
      - [`onUrlChange`](/packages/elm/browser/1.0.1/Browser#application) (when the browser actually changes the URL)
      - [`pushUrl`](/packages/elm/browser/latest/Browser-Navigation#pushUrl) (make the browser change the URL and add a new entry to the browser history)

@docs main

-}

import Api
import Api.Queries
import App
import Browser
import Browser.Navigation
import Html
import Types.Config as Config
import Types.Route as Route
import Types.Route.Url
import Types.ServerSetup exposing (ServerSetup)
import Url exposing (Url)


type alias Model =
    { navigationKey : Browser.Navigation.Key
    , app : App.Model
    }


{-| Define the application ingredients.
-}
main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , onUrlRequest = UrlRequest -- << Debug.log "onUrlRequest"
        , onUrlChange = UrlChanged -- << Debug.log "onUrlChange"
        }


type Msg
    = ApiResponseServerSetup Url (Api.Response ServerSetup)
    | UrlRequest Browser.UrlRequest
    | UrlChanged Url.Url
    | AppMsg App.Msg


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    ( { navigationKey = navigationKey
      , app = App.initEmptyModel
      }
    , Api.sendQueryRequest
        (Api.withOperationName "GetServerSetup")
        (ApiResponseServerSetup url)
        Api.Queries.serverSetup
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponseServerSetup url (Ok serverSetup) ->
            let
                ( appModel, appCmd ) =
                    Types.Route.Url.parseUrl Config.init url
                        |> Maybe.withDefault (Route.initHome Config.init)
                        |> Route.sanitize
                            (Config.updateFromServerSetup serverSetup Config.init)
                        |> App.initFromServerSetupAndRoute serverSetup
            in
            ( { model
                | app = appModel
              }
            , Cmd.batch
                [ Cmd.map AppMsg appCmd
                , -- Normalize the URL:
                  -- Replace the externally provided URL with one
                  -- that reflects the resulting intial state.
                  Browser.Navigation.replaceUrl
                    model.navigationKey
                    (Types.Route.Url.toString Config.init appModel.route)
                ]
            )

        ApiResponseServerSetup url (Err error) ->
            -- TODO
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
                    Types.Route.Url.parseUrl model.app.config url
                        |> Maybe.withDefault (Route.initHome model.app.config)
                        |> Route.sanitize model.app.config

                ( subModel, subCmd ) =
                    model.app
                        |> App.updateModelFromRoute route
                        |> App.requestNeeds
            in
            ( { model
                | app = subModel
              }
            , Cmd.map AppMsg subCmd
            )

        AppMsg subMsg ->
            let
                ( subModel1, subCmd1, subReturn1 ) =
                    App.update subMsg model.app

                ( subModel2, subCmd2 ) =
                    App.requestNeeds subModel1
            in
            ( { model
                | app = subModel2
              }
            , Cmd.batch
                [ Cmd.map AppMsg subCmd1
                , Cmd.map AppMsg subCmd2
                , case subReturn1 of
                    App.ReflectRoute route ->
                        Browser.Navigation.pushUrl
                            model.navigationKey
                            (Types.Route.Url.toString subModel2.config route)

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
