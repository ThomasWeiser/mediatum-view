module Setup exposing
    ( Model
    , Msg
    , Return(..)
    , initEmptyModel
    , initFromServerSetupAndRoute
    , requestNeeds
    , update
    , updateModelFromRoute
    , view
    )

{-| Top-level module sitting between Main and App, which manages setting up the client configuration
-}

import App
import Html exposing (Html)
import Types.Config as Config exposing (Config)
import Types.Route exposing (Route)
import Types.ServerSetup exposing (ServerSetup)


{-| Return value of the [`update`](#update) function.

Internal route changes are reported to the [`Main`](Main) module this way.

-}
type Return
    = NoReturn
    | ReflectRoute Route


{-| -}
type alias Model =
    { app : App.Model
    }


{-| `Msg` wraps the messages from the sub-component [`App`](App).
-}
type Msg
    = AppMsg App.Msg


{-| -}
initEmptyModel : Model
initEmptyModel =
    { app = App.initEmptyModel
    }


{-| Initialize the model with the given route and request the corresponding needs.
-}
initFromServerSetupAndRoute : ServerSetup -> Route -> ( Model, Cmd Msg )
initFromServerSetupAndRoute serverSetup route =
    let
        ( appModel, appCmd ) =
            App.initFromServerSetupAndRoute serverSetup route
    in
    ( { app = appModel }
    , Cmd.map AppMsg appCmd
    )


{-| Store a changed route and update the UI accordingly.
-}
updateModelFromRoute : Route -> Model -> Model
updateModelFromRoute route model =
    { model
        | app = App.updateModelFromRoute route model.app
    }


{-| Gather the needs from the UI and signal them to the cache.
-}
requestNeeds : Model -> ( Model, Cmd Msg )
requestNeeds model =
    let
        ( appModel, appCmd ) =
            App.requestNeeds model.app
    in
    ( { model | app = appModel }
    , Cmd.map AppMsg appCmd
    )


{-| Standard update function
-}
update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( appModel, appCmd, appReturn ) =
                    App.update appMsg model.app
            in
            ( { model | app = appModel }
            , Cmd.map AppMsg appCmd
            , case appReturn of
                App.NoReturn ->
                    NoReturn

                App.ReflectRoute route ->
                    ReflectRoute route
            )


{-| Standard view function
-}
view : Model -> Html Msg
view model =
    App.view
        model.app
        |> Html.map AppMsg
