port module Setup exposing
    ( Return(..), Model, Msg
    , init
    , requestNeeds, updateModelFromRoute, subscriptions, update, view
    )

{-| Top-level module sitting between Main and App, with the job to set up the client configuration

@docs Return, Model, Msg
@docs init
@docs requestNeeds, updateModelFromRoute, subscriptions, update, view

-}

import Api
import Api.Queries
import App
import Html exposing (Html)
import Json.Decode
import Types.Config as Config exposing (Config)
import Types.Localization as Localization
import Types.Route as Route exposing (Route)
import Types.ServerSetup exposing (ServerSetup)


{-| Return value of the [`update`](#update) function.

Internal route changes are reported to the [`Main`](Main) module this way.

-}
type Return
    = NoReturn
    | ReflectRoute Bool Route


{-| -}
type alias Model =
    { delayedInitWithRoute : Maybe Route
    , config : Config
    , app : App.Model
    }


{-| -}
type Msg
    = JSConfigChangeEvent Json.Decode.Value
    | ApiResponseServerSetup (Api.Response ServerSetup)
    | AppMsg App.Msg


port saveSelectedUILanguageTag : String -> Cmd msg


port jsConfigChangeEvent : (Json.Decode.Value -> msg) -> Sub msg


{-| Initialize fetching the ServerSetup as well as the App module

Note that the latter is delayed until config is known from received ServerSetup.

-}
init : Json.Decode.Value -> Route -> ( Model, Cmd Msg )
init flags route =
    ( { delayedInitWithRoute = Just route
      , config = Config.initFromFlags flags
      , app = App.initEmptyModel
      }
    , Api.sendQueryRequest
        (Api.withOperationName "GetServerSetup")
        ApiResponseServerSetup
        Api.Queries.serverSetup
    )


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    jsConfigChangeEvent JSConfigChangeEvent


{-| Report a changed route to the App and update it accordingly.
-}
updateModelFromRoute : Route -> Model -> Model
updateModelFromRoute route model =
    { model
        | app =
            App.updateModelFromRoute
                (appContext model)
                (Route.sanitize model.config route)
                model.app
    }


{-| Gather the needs from the App and signal them to the cache.
-}
requestNeeds : Model -> ( Model, Cmd Msg )
requestNeeds model =
    let
        ( appModel, appCmd ) =
            App.requestNeeds (appContext model) model.app
    in
    ( { model | app = appModel }
    , Cmd.map AppMsg appCmd
    )


{-| Standard update function
-}
update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    case msg of
        JSConfigChangeEvent eventJsonValue ->
            ( { model
                | config = Config.updateFromJSConfigChangeEvent eventJsonValue model.config
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseServerSetup (Ok serverSetup) ->
            let
                model1 =
                    { model
                        | config =
                            Config.updateFromServerSetup serverSetup model.config
                    }
            in
            case model1.delayedInitWithRoute of
                Nothing ->
                    ( model1
                    , Cmd.none
                    , NoReturn
                    )

                Just route ->
                    let
                        ( appModel, appCmd ) =
                            App.initFromRoute
                                (appContext model1)
                                (Route.sanitize model1.config route)
                    in
                    ( { model1
                        | delayedInitWithRoute = Nothing
                        , app = appModel
                      }
                    , Cmd.map AppMsg appCmd
                    , ReflectRoute False route
                    )

        ApiResponseServerSetup (Err error) ->
            -- TODO
            ( model, Cmd.none, NoReturn )

        AppMsg appMsg ->
            let
                ( appModel, appCmd, appReturn ) =
                    App.update (appContext model) appMsg model.app

                ( config, saveSelectedUILanguageCmd, return ) =
                    case appReturn of
                        App.NoReturn ->
                            ( model.config
                            , Cmd.none
                            , NoReturn
                            )

                        App.SwitchUILanguage language ->
                            ( model.config |> Config.setUiLanguage language
                            , saveSelectedUILanguageTag (Localization.languageToLanguageTag language)
                            , NoReturn
                            )

                        App.ReflectRoute route ->
                            ( model.config
                            , Cmd.none
                            , ReflectRoute True route
                            )
            in
            ( { model
                | config = config
                , app = appModel
              }
            , [ appCmd, saveSelectedUILanguageCmd ]
                |> Cmd.batch
                |> Cmd.map AppMsg
            , return
            )


{-| Standard view function
-}
view : Model -> Html Msg
view model =
    App.view
        (appContext model)
        model.app
        |> Html.map AppMsg


appContext : Model -> App.Context
appContext model =
    { config = model.config }
