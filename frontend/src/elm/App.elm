module App exposing
    ( Model
    , Msg
    , Return(..)
    , changeRouteTo
    , init
    , update
    , view
    )

import Api
import Api.Queries
import Article
import Cmd.Extra
import Controls
import Data.Cache as Cache
import Data.Types exposing (NodeId)
import Data.Utils
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
    , article = Article.initialModelEmpty
    , needs = Cache.NeedNothing
    }
        |> requestNeeds
        |> Cmd.Extra.andThen (changeRouteTo route >> Cmd.Extra.withNoCmd)


changeRouteTo : Route -> Model -> Model
changeRouteTo route model =
    let
        presentation =
            Presentation.fromRoute model.cache route
    in
    { model
        | route = route
        , presentation = presentation
        , controls = Controls.initialModel route
        , article =
            Article.initialModel
                { cache = model.cache, presentation = model.presentation }
    }


needs : Model -> Cache.Needs
needs model =
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
            { cache = model.cache }
            model.tree
        , Article.needs
            { cache = model.cache
            , presentation = model.presentation
            }
        ]
        |> Debug.log "App needs"


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

        ( model2, cmd2 ) =
            requestNeeds model1

        ( model3, return ) =
            case maybeNavigation of
                Nothing ->
                    ( model2, NoReturn )

                Just navigation ->
                    let
                        route =
                            Navigation.alterRoute
                                model2.cache
                                navigation
                                model2.route
                    in
                    ( changeRouteTo route { model2 | route = route }
                    , ReflectRoute route
                    )
    in
    ( model3
    , Cmd.batch [ cmd1, cmd2 ]
    , return
    )


updateSubModel : Msg -> Model -> ( Model, Cmd Msg, Maybe Navigation )
updateSubModel msg model =
    case msg of
        CacheMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Cache.update subMsg model.cache

                ( model1, cmd1 ) =
                    ( { model | cache = subModel }
                    , Cmd.map CacheMsg subCmd
                    )

                model2 =
                    case subReturn of
                        Cache.NoReturn ->
                            model1

                        Cache.GotRootFolders ->
                            let
                                firstRootFolderId =
                                    model1.cache.rootFolderIds
                                        |> RemoteData.withDefault []
                                        |> List.head
                            in
                            case firstRootFolderId of
                                Nothing ->
                                    model1

                                Just id ->
                                    { model1 | tree = Tree.showFolder id model1.tree }
            in
            ( model2
            , cmd1
            , Nothing
            )

        TreeMsg subMsg ->
            let
                ( subModel, subReturn ) =
                    Tree.update
                        { cache = model.cache }
                        subMsg
                        model.tree
            in
            ( { model | tree = subModel }
            , Cmd.none
            , case subReturn of
                Tree.UserSelection selectedFolder ->
                    Just (Navigation.FolderId selectedFolder)

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
                        { cache = model.cache }
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
