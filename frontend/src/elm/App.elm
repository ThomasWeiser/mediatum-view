module App exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , requestNeeds
    , update
    , updateRoute
    , view
    )

{-| Top-level module managing the interaction of the `Route`, the `Cache` and the `UI` components.

@docs Model
@docs Msg
@docs Return
@docs init
@docs requestNeeds
@docs update
@docs updateRoute
@docs view

-}

import Cache
import Cmd.Extra
import Html exposing (Html)
import Types.DebugInfo exposing (DebugInfo, debugInfo)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation as Presentation exposing (Presentation(..))
import Types.Route as Route exposing (Route)
import UI


{-| Return value of the [`update`](#update) function.

Internal route changes are reported to the [`Main`](Main) module this way.

-}
type Return
    = NoReturn
    | ReflectRoute Route


{-| The model of the app comprises the page's [`Route`](Types-Route), the content data currently in the [`Cache`](Cache) and the [`UI`](UI) state.

The [`Presentation`](Types-Presentation) is another representation of the route that uses some knowledge from the cache.
Although not strictly necessary we store it in the model too as this leads to simpler code.

Finally there is some [`DebugInfo`](Types-DebugInfo) here for inspection by the Elm debugger.

-}
type alias Model =
    { route : Route
    , cache : Cache.Model
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


{-| Initialize the model with the given route and request the corresponding needs.
-}
init : Route -> ( Model, Cmd Msg )
init route =
    { route = Route.initHome
    , cache = Cache.initialModel
    , presentation = GenericPresentation Nothing
    , ui = UI.init
    , debugInfo = { needs = debugInfo Cache.NeedNothing }
    }
        |> requestNeeds
        |> Cmd.Extra.andThen (updateRoute route >> Cmd.Extra.withNoCmd)


{-| Store a changed route and update the UI accordingly.
-}
updateRoute : Route -> Model -> Model
updateRoute route model =
    let
        model1 =
            { model | route = route }

        model2 =
            { model1
                | ui =
                    UI.updateOnChangedRoute
                        (uiContext model1)
                        model1.ui
            }

        model3 =
            adjustPresentation model2
    in
    model3


{-| Derive the current presentation from the route using contextual knowledge from the cache and update the UI accordingly.
-}
adjustPresentation : Model -> Model
adjustPresentation model =
    let
        presentation =
            Presentation.fromRoute
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
requestNeeds : Model -> ( Model, Cmd Msg )
requestNeeds model =
    let
        currentNeeds =
            UI.needs (uiContext model) model.ui

        ( cacheModel, cacheCmd ) =
            Cache.require
                currentNeeds
                model.cache
    in
    ( { model
        | debugInfo = { needs = debugInfo currentNeeds }
        , cache = cacheModel
      }
    , Cmd.map CacheMsg cacheCmd
    )


{-| Standard update function
-}
update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    let
        ( model1, cmd1, maybeNavigation ) =
            updateSubModel msg model

        ( model2, return ) =
            case maybeNavigation of
                Nothing ->
                    ( model1, NoReturn )

                Just navigation ->
                    let
                        route =
                            Navigation.alterRoute
                                model1.cache
                                navigation
                                model1.route
                    in
                    ( updateRoute route model1
                    , ReflectRoute route
                    )
    in
    ( model2
    , cmd1
    , return
    )


updateSubModel : Msg -> Model -> ( Model, Cmd Msg, Maybe Navigation )
updateSubModel msg model =
    case msg of
        CacheMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Cache.update subMsg model.cache

                ( model1, cmd1 ) =
                    ( { model | cache = subModel }
                    , Cmd.map CacheMsg subCmd
                    )
            in
            ( model1 |> adjustPresentation
            , cmd1
            , Nothing
            )

        UIMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    UI.update
                        (uiContext model)
                        subMsg
                        model.ui

                model1 =
                    { model | ui = subModel }

                ( model2, maybeNavigation ) =
                    case subReturn of
                        UI.NoReturn ->
                            ( model1, Nothing )

                        UI.Navigate navigation ->
                            ( model1, Just navigation )

                        UI.UpdateCacheWithModifiedDocument document ->
                            ( { model1
                                | cache = Cache.updateWithModifiedDocument document model1.cache
                              }
                            , Nothing
                            )
            in
            ( model2
            , Cmd.map UIMsg subCmd
            , maybeNavigation
            )


{-| Standard view function
-}
view : Model -> Html Msg
view model =
    UI.view
        (uiContext model)
        model.ui
        |> Html.map UIMsg


uiContext : Model -> UI.Context
uiContext model =
    { cache = model.cache
    , route = model.route
    , presentation = model.presentation
    }
