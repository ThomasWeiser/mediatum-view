module App exposing
    ( Context, Return(..), Model, Msg
    , initEmptyModel, initFromRoute
    , requestNeeds, updateModelFromRoute, update, view
    )

{-| Top-level module managing the interaction of the `Route`, the `Cache` and the `UI` components.

@docs Context, Return, Model, Msg
@docs initEmptyModel, initFromRoute
@docs requestNeeds, updateModelFromRoute, update, view

-}

import Cache exposing (Cache)
import Cmd.Extra
import Html exposing (Html)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.DebugInfo exposing (DebugInfo, debugInfo)
import Types.Localization exposing (Language)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Needs
import Types.Presentation as Presentation exposing (Presentation(..))
import Types.Route as Route exposing (Route)
import UI exposing (Return(..))


{-| Context data provided by the parent module [`Setup`](Setup).
-}
type alias Context =
    { config : Config
    }


{-| Return value of the [`update`](#update) function.

Internal route changes are reported to the [`Main`](Main) module this way.

-}
type Return
    = NoReturn
    | SwitchUILanguage Language
    | ReflectRoute Route


{-| The model of the app comprises the page's [`Route`](Types-Route), the content data currently in the [`Cache`](Cache) and the [`UI`](UI) state.

The [`Presentation`](Types-Presentation) is another representation of the route that uses some knowledge from the cache.
Although not strictly necessary we store it in the model too as this leads to simpler code.

Finally there is some [`DebugInfo`](Types-DebugInfo) here for inspection by the Elm debugger.

-}
type alias Model =
    { route : Route
    , cache : Cache
    , presentation : Presentation
    , ui : UI.Model

    -- TODO: We store the `Needs` here only for debugging
    , debugInfo : { needs : DebugInfo Cache.Needs }
    }


{-| `Msg` wraps the messages from the two sub-components [`Cache`](Cache) and [`UI`](UI).
-}
type Msg
    = CacheMsg Cache.Msg
    | UIMsg UI.Msg


{-| Initialize the model with no knowledge of route or content.
-}
initEmptyModel : Model
initEmptyModel =
    { route = Route.initHome Config.init
    , cache = Cache.init
    , presentation = GenericPresentation Nothing
    , ui = UI.init Config.init
    , debugInfo = { needs = debugInfo Types.Needs.none }
    }


{-| Initialize the model with the given route and request the corresponding needs.
-}
initFromRoute : Context -> Route -> ( Model, Cmd Msg )
initFromRoute context route =
    { route = Route.initHome context.config
    , cache = Cache.init
    , presentation = GenericPresentation Nothing
    , ui = UI.init context.config
    , debugInfo = { needs = debugInfo Types.Needs.none }
    }
        |> requestNeeds context
        |> Cmd.Extra.andThen (updateModelFromRoute context route >> Cmd.Extra.withNoCmd)


{-| Store a changed route and update the UI accordingly.
-}
updateModelFromRoute : Context -> Route -> Model -> Model
updateModelFromRoute context route model =
    let
        model1 =
            { model | route = route }

        model2 =
            { model1
                | ui =
                    UI.updateOnChangedRoute
                        (uiContext context model1)
                        model1.ui
            }

        model3 =
            adjustPresentation context.config model2
    in
    model3


{-| Derive the current presentation from the route using contextual knowledge from the cache and update the UI accordingly.
-}
adjustPresentation : Config -> Model -> Model
adjustPresentation config model =
    let
        presentation =
            Presentation.fromRoute
                config
                model.cache
                model.route
    in
    -- This function will be called each time the cache gets a message.
    -- Let's call `UI.updateOnChangedPresentation` only if the presentation has really changed.
    if presentation == model.presentation then
        model

    else
        { model
            | presentation = presentation
            , ui =
                UI.updateOnChangedPresentation
                    presentation
                    model.ui
        }


{-| Gather the needs from the UI and signal them to the cache.
-}
requestNeeds : Context -> Model -> ( Model, Cmd Msg )
requestNeeds context model =
    let
        currentNeeds =
            UI.needs (uiContext context model) model.ui

        ( cacheModel, cacheCmd ) =
            Cache.targetNeeds
                context.config
                currentNeeds
                model.cache
    in
    ( { model
        | debugInfo =
            { needs = debugInfo (Types.Needs.flatten currentNeeds) }
        , cache = cacheModel
      }
    , Cmd.map CacheMsg cacheCmd
    )


{-| Standard update function
-}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    let
        ( model1, cmd1, subReturn ) =
            updateSubModel context msg model

        ( model2, return ) =
            case subReturn of
                SubNoReturn ->
                    ( model1, NoReturn )

                SubReturnSwitchUILanguage language ->
                    ( model1, SwitchUILanguage language )

                SubReturnNavigate navigation ->
                    let
                        route =
                            Navigation.alterRoute
                                context.config
                                model1.cache
                                navigation
                                model1.route
                    in
                    ( updateModelFromRoute context route model1
                    , ReflectRoute route
                    )
    in
    ( model2
    , cmd1
    , return
    )


type SubReturn
    = SubNoReturn
    | SubReturnNavigate Navigation
    | SubReturnSwitchUILanguage Language


updateSubModel : Context -> Msg -> Model -> ( Model, Cmd Msg, SubReturn )
updateSubModel context msg model =
    case msg of
        CacheMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Cache.update context.config subMsg model.cache

                ( model1, cmd1 ) =
                    ( { model | cache = subModel }
                    , Cmd.map CacheMsg subCmd
                    )
            in
            ( model1 |> adjustPresentation context.config
            , cmd1
            , SubNoReturn
            )

        UIMsg subMsg ->
            let
                ( uiModel, uiCmd, uiReturn ) =
                    UI.update
                        (uiContext context model)
                        subMsg
                        model.ui

                model1 =
                    { model | ui = uiModel }

                ( model2, subReturn ) =
                    case uiReturn of
                        UI.NoReturn ->
                            ( model1, SubNoReturn )

                        UI.Navigate navigation ->
                            ( model1, SubReturnNavigate navigation )

                        UI.UpdateCacheWithModifiedDocument document ->
                            ( { model1
                                | cache =
                                    Cache.updateWithModifiedDocument
                                        (Config.getMaskName MasksConfig.MaskForDetails context.config)
                                        document
                                        model1.cache
                              }
                            , SubNoReturn
                            )

                        UI.SwitchUILanguage language ->
                            ( model1, SubReturnSwitchUILanguage language )
            in
            ( model2
            , Cmd.map UIMsg uiCmd
            , subReturn
            )


{-| Standard view function
-}
view : Context -> Model -> Html Msg
view context model =
    UI.view
        (uiContext context model)
        model.ui
        |> Html.map UIMsg


uiContext : Context -> Model -> UI.Context
uiContext context model =
    { config = context.config
    , cache = model.cache
    , route = model.route
    , presentation = model.presentation
    }
