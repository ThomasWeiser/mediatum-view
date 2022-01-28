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
import Json.Decode as JD
import Maybe.Extra
import Types.AdjustmentToSetup as AdjustmentToSetup exposing (AdjustmentToSetup)
import Types.Config as Config exposing (Config)
import Types.Localization as Localization exposing (Language)
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
    { navigatorLanguage : Maybe Language
    , userSelectedUILanguage : Maybe Language
    , delayedInitWithRoute : Maybe Route
    , config : Config
    , app : App.Model
    }


{-| -}
type Msg
    = JSConfigChange JD.Value
    | ApiResponseServerSetup (Api.Response ServerSetup)
    | AppMsg App.Msg


port saveSelectedUILanguageTag : String -> Cmd msg


port jsConfigChange : (JD.Value -> msg) -> Sub msg


{-| Initialize fetching the ServerSetup as well as the App module

Note that the latter is delayed until config is known from received ServerSetup.

-}
init : JD.Value -> Route -> ( Model, Cmd Msg )
init flagsJsonValue route =
    let
        flags =
            decodeFlags flagsJsonValue

        config =
            Config.init
                |> Config.adjustUILanguage
                    flags.navigatorLanguage
                    flags.storedSelectedUILanguage
    in
    ( { navigatorLanguage = flags.navigatorLanguage
      , userSelectedUILanguage = flags.storedSelectedUILanguage
      , delayedInitWithRoute = Just route
      , config = config
      , app = App.initEmptyModel
      }
    , Api.sendQueryRequest
        (Api.withOperationName "GetServerSetup")
        ApiResponseServerSetup
        Api.Queries.serverSetup
    )


{-| Parameters passed from JS on startup
-}
type alias Flags =
    { navigatorLanguage : Maybe Language
    , storedSelectedUILanguage : Maybe Language
    }


decodeFlags : JD.Value -> Flags
decodeFlags flagsJsonValue =
    let
        decoder =
            JD.map2 Flags
                (JD.field "navigatorLanguageTag" JD.string
                    |> JD.maybe
                    |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
                )
                (JD.field "userSelectedUILanguageTag" JD.string
                    |> JD.maybe
                    |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
                )
    in
    JD.decodeValue decoder flagsJsonValue
        |> Result.withDefault
            { navigatorLanguage = Nothing
            , storedSelectedUILanguage = Nothing
            }


{-| Change-events passed from JS
-}
type alias JSConfigChangeEvent =
    { changedNavigatorLanguage : Maybe Language
    , changedSelectedUILanguage : Maybe Language
    }


decodeConfigChangeEvent : JD.Value -> JSConfigChangeEvent
decodeConfigChangeEvent eventJsonValue =
    let
        decoder =
            JD.map2 JSConfigChangeEvent
                (JD.field "changedNavigatorLanguageTag" JD.string
                    |> JD.maybe
                    |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
                )
                (JD.field "changedSelectedUILanguageTag" JD.string
                    |> JD.maybe
                    |> JD.map (Maybe.andThen Localization.languageFromLanguageTag)
                )
    in
    JD.decodeValue decoder eventJsonValue
        |> Result.withDefault
            { changedNavigatorLanguage = Nothing
            , changedSelectedUILanguage = Nothing
            }


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    jsConfigChange JSConfigChange


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


{-| -}
adjust : AdjustmentToSetup -> Model -> ( Model, Cmd Msg )
adjust adjustment model =
    case adjustment of
        AdjustmentToSetup.UserSelectedUILanguage userSelectedUILanguage ->
            ( { model
                | userSelectedUILanguage = Just userSelectedUILanguage
                , config =
                    model.config
                        |> Config.adjustUILanguage
                            model.navigatorLanguage
                            (Just userSelectedUILanguage)
              }
            , saveSelectedUILanguageTag
                (Localization.languageToLanguageTag userSelectedUILanguage)
            )

        AdjustmentToSetup.HideThumbnails state ->
            ( { model
                | config = model.config |> Config.adjustHideThumbnails state
              }
            , Cmd.none
            )

        AdjustmentToSetup.HideSidebar state ->
            ( { model
                | config = model.config |> Config.adjustHideSidebar state
              }
            , Cmd.none
            )


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
        JSConfigChange eventJsonValue ->
            let
                event =
                    decodeConfigChangeEvent eventJsonValue

                model1 =
                    { model
                        | navigatorLanguage =
                            Maybe.Extra.or
                                event.changedNavigatorLanguage
                                model.navigatorLanguage
                        , userSelectedUILanguage =
                            Maybe.Extra.or
                                event.changedSelectedUILanguage
                                model.userSelectedUILanguage
                    }
            in
            ( { model1
                | config =
                    model1.config
                        |> Config.adjustUILanguage
                            model1.navigatorLanguage
                            model1.userSelectedUILanguage
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

                ( model1, adjustSetupCmd, return ) =
                    case appReturn of
                        App.NoReturn ->
                            ( model
                            , Cmd.none
                            , NoReturn
                            )

                        App.AdjustSetup adjustment ->
                            let
                                ( model1a, cmd1 ) =
                                    adjust adjustment model
                            in
                            ( model1a
                            , cmd1
                            , NoReturn
                            )

                        App.ReflectRoute route ->
                            ( model
                            , Cmd.none
                            , ReflectRoute True route
                            )
            in
            ( { model1
                | app = appModel
              }
            , Cmd.batch
                [ appCmd |> Cmd.map AppMsg
                , adjustSetupCmd
                ]
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
