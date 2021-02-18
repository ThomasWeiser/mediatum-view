module Main exposing (main)

{-| The `Main` module is responsible for:

  - Initializing the application as a [`Browser.application`](/packages/elm/browser/1.0.1/Browser#application) that takes full control over the browser page.

  - Hosting the module [`Setup`](Setup), which in turn hosts the module [`App`](App).

  - Mananging URL changes by means of:
      - [`onUrlRequest`](/packages/elm/browser/1.0.1/Browser#application) (when the user clicks a link)
      - [`onUrlChange`](/packages/elm/browser/1.0.1/Browser#application) (when the browser actually changes the URL)
      - [`pushUrl`](/packages/elm/browser/latest/Browser-Navigation#pushUrl) (make the browser change the URL and add a new entry to the browser history)

@docs main

-}

import Api
import Api.Queries
import Browser
import Browser.Navigation
import Html
import Setup
import Types.Config as Config
import Types.Route as Route
import Types.Route.Url
import Types.ServerSetup exposing (ServerSetup)
import Url exposing (Url)


type alias Model =
    { navigationKey : Browser.Navigation.Key
    , setup : Setup.Model
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
    | SetupMsg Setup.Msg


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    ( { navigationKey = navigationKey
      , setup = Setup.initEmptyModel
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
                ( setupModel, setupCmd ) =
                    Types.Route.Url.parseUrl Config.init url
                        |> Maybe.withDefault (Route.initHome Config.init)
                        |> Route.sanitize
                            (Config.updateFromServerSetup serverSetup Config.init)
                        |> Setup.initFromServerSetupAndRoute serverSetup
            in
            ( { model
                | setup = setupModel
              }
            , Cmd.batch
                [ Cmd.map SetupMsg setupCmd
                , -- Normalize the URL:
                  -- Replace the externally provided URL with one
                  -- that reflects the resulting intial state.
                  Browser.Navigation.replaceUrl
                    model.navigationKey
                    (Types.Route.Url.toString Config.init
                        -- TODO: Kein Durchgriff auf app?
                        setupModel.app.route
                    )
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
                    Types.Route.Url.parseUrl model.setup.config url
                        |> Maybe.withDefault (Route.initHome model.setup.config)
                        |> Route.sanitize model.setup.config

                ( setupModel, setupCmd ) =
                    model.setup
                        |> Setup.updateModelFromRoute route
                        |> Setup.requestNeeds
            in
            ( { model
                | setup = setupModel
              }
            , Cmd.map SetupMsg setupCmd
            )

        SetupMsg setupMsg ->
            let
                ( setupModel1, setupCmd1, setupReturn ) =
                    Setup.update setupMsg model.setup

                ( setupModel2, setupCmd2 ) =
                    Setup.requestNeeds setupModel1
            in
            ( { model
                | setup = setupModel2
              }
            , Cmd.batch
                [ Cmd.map SetupMsg setupCmd1
                , Cmd.map SetupMsg setupCmd2
                , case setupReturn of
                    Setup.ReflectRoute route ->
                        Browser.Navigation.pushUrl
                            model.navigationKey
                            (Types.Route.Url.toString setupModel2.config route)

                    Setup.NoReturn ->
                        Cmd.none
                ]
            )


view : Model -> Browser.Document Msg
view model =
    { title = "mediaTUM View"
    , body =
        [ Setup.view model.setup
            |> Html.map SetupMsg
        ]
    }
