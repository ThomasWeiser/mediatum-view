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
import Api.Queries exposing (serverConfig)
import App
import Browser
import Browser.Navigation
import Html
import Types.Route as Route
import Types.Route.Url
import Types.ServerConfig exposing (ServerConfig)
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
    = ApiResponseServerConfig Url (Api.Response ServerConfig)
    | UrlRequest Browser.UrlRequest
    | UrlChanged Url.Url
    | AppMsg App.Msg


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    ( { navigationKey = navigationKey
      , app = App.initEmptyModel
      }
    , Api.sendQueryRequest
        (Api.withOperationName "GetServerConfig")
        (ApiResponseServerConfig url)
        Api.Queries.serverConfig
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponseServerConfig url (Ok serverConfig) ->
            let
                ( appModel, appCmd ) =
                    Types.Route.Url.parseUrl serverConfig.defaults url
                        |> Maybe.withDefault (Route.initHome serverConfig.defaults)
                        |> Route.sanitize
                        |> App.initFromServerConfigAndRoute serverConfig
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
                    (Types.Route.Url.toString serverConfig.defaults appModel.route)
                ]
            )

        ApiResponseServerConfig url (Err error) ->
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
                    Types.Route.Url.parseUrl model.app.serverConfig.defaults url
                        |> Maybe.withDefault (Route.initHome model.app.serverConfig.defaults)
                        |> Route.sanitize

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
                            (Types.Route.Url.toString subModel2.serverConfig.defaults route)

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
