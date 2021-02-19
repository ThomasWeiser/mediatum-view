module Setup exposing
    ( Return(..), Model, Msg
    , init
    , requestNeeds, updateModelFromRoute, update, view
    )

{-| Top-level module sitting between Main and App, to set up the client configuration

@docs Return, Model, Msg
@docs init
@docs requestNeeds, updateModelFromRoute, update, view

-}

import Api
import Api.Queries
import App
import Html exposing (Html)
import Types.Config as Config exposing (Config)
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
    = ApiResponseServerSetup (Api.Response ServerSetup)
    | AppMsg App.Msg


{-| Initialize fetching the ServerSetup as well as the App module

Note that the latter is delayed until config is known from received ServerSetup.

-}
init : Route -> ( Model, Cmd Msg )
init route =
    ( { delayedInitWithRoute = Just route
      , config = Config.init
      , app = App.initEmptyModel
      }
    , Api.sendQueryRequest
        (Api.withOperationName "GetServerSetup")
        ApiResponseServerSetup
        Api.Queries.serverSetup
    )


{-| Store a changed route and update the UI accordingly.
-}
updateModelFromRoute : Route -> Model -> Model
updateModelFromRoute route model =
    { model
        | app = App.updateModelFromRoute (appContext model) route model.app
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
            in
            ( { model | app = appModel }
            , Cmd.map AppMsg appCmd
            , case appReturn of
                App.NoReturn ->
                    NoReturn

                App.ReflectRoute route ->
                    ReflectRoute True route
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
