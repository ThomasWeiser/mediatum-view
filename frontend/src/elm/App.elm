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

import Api
import Api.Queries
import Article
import Cmd.Extra
import Controls
import Data.Cache as Cache
import Data.Types exposing (NodeId)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Navigation exposing (Navigation)
import Presentation exposing (Presentation(..))
import Query.Filters
import RemoteData
import Route exposing (Route)
import Sort.Dict
import Tree
import Utils


type Return
    = NoReturn
    | ReflectRoute Route


type alias Model =
    { route : Route
    , cache : Cache.Model
    , presentation : Presentation
    , tree : Tree.Model
    , controls : Controls.Model
    , article : Article.Model

    -- TODO: We store the Needs here only for debugging
    , needs : Cache.Needs
    }


type Msg
    = CacheMsg Cache.Msg
    | TreeMsg Tree.Msg
    | ControlsMsg Controls.Msg
    | ArticleMsg Article.Msg


init : Route -> ( Model, Cmd Msg )
init route =
    { route = Route.home
    , cache = Cache.initialModel
    , presentation = GenericPresentation Nothing
    , tree = Tree.initialModel
    , controls = Controls.initialModel Route.home
    , article =
        Article.initialModel
            { cache = Cache.initialModel
            , presentation = GenericPresentation Nothing
            }
    , needs = Cache.NeedNothing
    }
        |> requestNeeds
        |> Cmd.Extra.andThen (updateRoute route >> Cmd.Extra.withNoCmd)


updateRoute : Route -> Model -> Model
updateRoute route model =
    let
        model1 =
            { model
                | route = route
                , controls = Controls.initialModel route
            }

        model2 =
            adjust model1

        model3 =
            { model2
                | tree = Tree.expandPresentationFolder model2.tree
            }
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
        , article =
            Article.initialModel
                { cache = model.cache, presentation = presentation }
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
                        (Cache.NeedGenericNode nodeIdOne)
            , Tree.needs
                { cache = model.cache
                , presentation = model.presentation
                }
                model.tree
            , Article.needs
                { cache = model.cache
                , presentation = model.presentation
                }
            ]


requestNeeds : Model -> ( Model, Cmd Msg )
requestNeeds model =
    let
        currentNeeds =
            needs model

        ( cacheModel, cacheCmd ) =
            Cache.requestNeeds
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

        TreeMsg subMsg ->
            let
                ( subModel, subReturn ) =
                    Tree.update
                        { cache = model.cache
                        , presentation = model.presentation
                        }
                        subMsg
                        model.tree
            in
            ( { model | tree = subModel }
            , Cmd.none
            , case subReturn of
                Tree.UserSelection selectedFolder ->
                    Just (Navigation.ShowListingWithFolder selectedFolder)

                Tree.NoReturn ->
                    Nothing
            )

        ControlsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Controls.update
                        { route = model.route
                        , presentation = model.presentation
                        }
                        subMsg
                        model.controls
            in
            ( { model | controls = subModel }
            , Cmd.map ControlsMsg subCmd
            , case subReturn of
                Controls.NoReturn ->
                    Nothing

                Controls.Navigate navigation ->
                    Just navigation
            )

        ArticleMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Article.update
                        { cache = model.cache
                        , presentation = model.presentation
                        }
                        subMsg
                        model.article

                model1 =
                    { model | article = subModel }

                ( model2, maybeNavigation ) =
                    case subReturn of
                        Article.NoReturn ->
                            ( model1, Nothing )

                        Article.Navigate navigation ->
                            ( model1, Just navigation )

                        Article.UpdateCacheWithModifiedDocument document ->
                            ( { model1
                                | cache = Cache.updateWithModifiedDocument document model1.cache
                              }
                            , Nothing
                            )
            in
            ( model2
            , Cmd.map ArticleMsg subCmd
            , maybeNavigation
            )


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "page-container" ]
        [ Icons.definitions
        , Html.header []
            [ Html.h2 []
                [ Html.div []
                    [ Html.a
                        [ Html.Attributes.class "title"
                        , Html.Attributes.href "/"
                        ]
                        [ Html.text "mediaTUM view" ]
                    , Html.span
                        [ Html.Attributes.class "subtitle"
                        , Html.Attributes.title "You may click here to start an example query."
                        , Html.Events.onClick (ControlsMsg Controls.submitExampleQuery)
                        ]
                        [ Html.text "WIP" ]
                    , Html.img
                        [ Html.Attributes.alt "TUM Logo"
                        , Html.Attributes.src "logo_tum.png"
                        , Html.Attributes.style "float" "right"
                        ]
                        []
                    ]
                ]
            , Controls.view
                { route = model.route
                , presentation = model.presentation
                }
                model.controls
                |> Html.map ControlsMsg
            ]
        , Html.main_ []
            [ Html.aside []
                [ Html.map TreeMsg <|
                    Tree.view
                        { cache = model.cache
                        , presentation = model.presentation
                        }
                        model.tree
                        (Article.folderCountsForQuery
                            { cache = model.cache
                            , presentation = model.presentation
                            }
                        )
                ]
            , Html.map ArticleMsg <|
                Article.view
                    model.tree
                    { cache = model.cache
                    , presentation = model.presentation
                    }
                    model.article
            ]
        ]
