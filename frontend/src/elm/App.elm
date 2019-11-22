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

import Cache
import Cmd.Extra
import Html exposing (Html)
import Navigation exposing (Navigation)
import Presentation exposing (Presentation(..))
import Route exposing (Route)
import UI


type Return
    = NoReturn
    | ReflectRoute Route


type alias Model =
    { route : Route
    , cache : Cache.Model
    , presentation : Presentation
    , ui : UI.Model

    -- TODO: We store the Needs here only for debugging
    , needs : Cache.Needs
    }


type Msg
    = CacheMsg Cache.Msg
    | UIMsg UI.Msg


init : Route -> ( Model, Cmd Msg )
init route =
    { route = Route.initHome
    , cache = Cache.initialModel
    , presentation = GenericPresentation Nothing
    , ui = UI.init
    , needs = Cache.NeedNothing
    }
        |> requestNeeds
        |> Cmd.Extra.andThen (updateRoute route >> Cmd.Extra.withNoCmd)


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
            adjust model2
    in
    model3


adjust : Model -> Model
adjust model =
    let
        presentation =
            model.route
                |> Debug.log "adjust route"
                |> Presentation.fromRoute model.cache
                |> Debug.log "adjust presentation"
                |> identity
    in
    { model
        | presentation = presentation
        , ui =
            UI.adjust
                (uiContext model)
                model.ui
    }


needs : Model -> Cache.Needs
needs model =
    Debug.log "app needs" <|
        Cache.needsFromList
            [ Cache.NeedRootFolderIds
            , case model.route.path of
                Route.NoId ->
                    Cache.NeedNothing

                Route.OneId nodeId ->
                    Cache.NeedGenericNode nodeId

                Route.TwoIds nodeIdOne nodeIdTwo ->
                    Cache.NeedAnd
                        (Cache.NeedGenericNode nodeIdOne)
                        (Cache.NeedGenericNode nodeIdTwo)
            , UI.needs
                (uiContext model)
                model.ui
            ]


requestNeeds : Model -> ( Model, Cmd Msg )
requestNeeds model =
    let
        currentNeeds =
            needs model

        ( cacheModel, cacheCmd ) =
            Cache.require
                currentNeeds
                model.cache
    in
    ( { model
        | needs = currentNeeds
        , cache = cacheModel
      }
    , Cmd.map CacheMsg cacheCmd
    )


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
            ( model1 |> adjust
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
