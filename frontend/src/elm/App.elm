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
import GenericNode exposing (GenericNode)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Query exposing (Query)
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
    , query : Query
    , tree : Tree.Model
    , controls : Controls.Model
    , article : Article.Model

    -- TODO: We store the Needs here only for debugging
    , needs : Cache.Needs
    }


type Msg
    = NoOp
    | QueryGenericNode NodeId
    | GotRootFolders
    | GenericNodeQueryResponse (Api.Response GenericNode)
    | CacheMsg Cache.Msg
    | TreeMsg Tree.Msg
    | ControlsMsg Controls.Msg
    | ArticleMsg Article.Msg


init : Route -> ( Model, Cmd Msg )
init route =
    { route = Route.Invalid "to be initialized"
    , cache = Cache.initialModel
    , query = Query.emptyQuery
    , tree = Tree.initialModel
    , controls = Controls.init ()
    , article = Article.initialModelEmpty
    , needs = Cache.NeedNothing
    }
        |> requestNeeds
        |> Cmd.Extra.andThen (changeRouteTo route)


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        model1 =
            { model | route = route }
    in
    case route of
        Route.Home ->
            ( model1, Cmd.none )

        Route.NodeId nodeId ->
            if model.route /= route then
                updateWithoutReturn
                    (QueryGenericNode nodeId)
                    model1

            else
                ( model1, Cmd.none )

        Route.Invalid errorMsg ->
            ( model1, Cmd.none )


needs : Model -> Cache.Needs
needs model =
    Cache.NeedListOfNeeds
        [ Cache.NeedRootFolderIds
        , case model.route of
            Route.NodeId nodeId ->
                Cache.NeedGenericNode nodeId

            _ ->
                Cache.NeedNothing
        , Tree.needs { cache = model.cache } model.tree
        , Article.needs model.query
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
        ( model1, cmd ) =
            updateWithoutReturn msg model
                |> Cmd.Extra.andThen requestNeeds
    in
    ( model1
    , cmd
    , if model1.route /= model.route then
        ReflectRoute model1.route

      else
        NoReturn
    )


updateWithoutReturn : Msg -> Model -> ( Model, Cmd Msg )
updateWithoutReturn msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        QueryGenericNode nodeId ->
            ( model
            , Api.sendQueryRequest
                GenericNodeQueryResponse
                (Api.Queries.genericNode nodeId)
            )

        GenericNodeQueryResponse (Err err) ->
            let
                -- TODO
                _ =
                    Debug.log "GenericNodeQueryResponse" err
            in
            ( model, Cmd.none )

        GenericNodeQueryResponse (Ok genericNode) ->
            case genericNode of
                GenericNode.IsFolder lineage ->
                    startQuery
                        (Query.OnFolder
                            { folder = List.Nonempty.head lineage
                            , filters = Query.getFilters model.query
                            , window = Query.initialWindow
                            }
                        )
                        { model
                            | tree =
                                Tree.showFolder
                                    (List.Nonempty.head lineage |> .id)
                                    model.tree
                        }

                GenericNode.IsDocument document ->
                    -- Currently we fetch the document once again here,
                    -- which is not a big deal anyway.
                    -- Will decide later what we really want here.
                    startQuery
                        (Query.OnDetails
                            { folder = Query.getFolder model.query
                            , documentId = document.id
                            , filters = Query.getFilters model.query
                            }
                        )
                        model

                GenericNode.IsNeither ->
                    -- TODO
                    ( model, Cmd.none )

        GotRootFolders ->
            let
                firstRootFolderId =
                    model.cache.rootFolderIds
                        |> RemoteData.withDefault []
                        |> List.head

                model1 =
                    case firstRootFolderId of
                        Nothing ->
                            model

                        Just id ->
                            { model | tree = Tree.showFolder id model.tree }
            in
            case
                ( model1.route
                , firstRootFolderId
                    |> Maybe.andThen
                        (Cache.get model1.cache.folders
                            >> RemoteData.toMaybe
                        )
                )
            of
                ( Route.Home, Just rootFolderToBeQueried ) ->
                    startQuery
                        (Query.setFolder
                            rootFolderToBeQueried
                            model1.query
                        )
                        model1

                ( _, maybeRootFolder ) ->
                    ( { model1
                        | query =
                            Query.stopgapFolder
                                maybeRootFolder
                                model1.query
                      }
                    , Cmd.none
                    )

        CacheMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Cache.update subMsg model.cache

                ( model1, cmd1 ) =
                    ( { model | cache = subModel }
                    , Cmd.map CacheMsg subCmd
                    )

                ( model2, cmd2 ) =
                    case subReturn of
                        Cache.NoReturn ->
                            ( model1, Cmd.none )

                        Cache.GotRootFolders ->
                            updateWithoutReturn GotRootFolders model1
            in
            ( model2
            , Cmd.batch [ cmd1, cmd2 ]
            )

        TreeMsg subMsg ->
            let
                ( subModel, subReturn ) =
                    Tree.update
                        { cache = model.cache }
                        subMsg
                        model.tree

                model1 =
                    { model | tree = subModel }
            in
            case subReturn of
                Tree.UserSelection selectedFolder ->
                    startQuery
                        (Query.setFolder
                            selectedFolder
                            model1.query
                        )
                        model1

                Tree.NoReturn ->
                    ( model1, Cmd.none )

        ControlsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Controls.update
                        { query = model.query }
                        subMsg
                        model.controls

                model1 =
                    { model | controls = subModel }
            in
            (case subReturn of
                Controls.NoReturn ->
                    ( model1, Cmd.none )

                Controls.MapQuery queryMapping ->
                    startQuery (queryMapping model1.query) model1
            )
                |> Cmd.Extra.addCmd (Cmd.map ControlsMsg subCmd)

        ArticleMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Article.update
                        { cache = model.cache
                        , query = model.query
                        }
                        subMsg
                        model.article

                model1 =
                    { model | article = subModel }
            in
            (case subReturn of
                Article.NoReturn ->
                    ( model1
                    , Cmd.none
                    )

                Article.MapQuery queryMapping ->
                    startQuery (queryMapping model1.query) model1

                Article.UpdateCacheWithModifiedDocument document ->
                    ( { model
                        | cache = Cache.updateWithModifiedDocument document model.cache
                      }
                    , Cmd.none
                    )
            )
                |> Cmd.Extra.addCmd (Cmd.map ArticleMsg subCmd)


startQuery : Query -> Model -> ( Model, Cmd Msg )
startQuery query model =
    let
        ( articleModel, articleCmd ) =
            Article.initWithQuery
                { cache = model.cache
                , query = query
                }
    in
    ( { model
        | route = Query.toRoute query
        , query = Debug.log "startQuery" query
        , article = articleModel
      }
    , Cmd.map ArticleMsg articleCmd
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
            , Controls.view { query = model.query } model.controls
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
                            , query = model.query
                            }
                        )
                ]
            , Html.map ArticleMsg <|
                Article.view
                    model.tree
                    { cache = model.cache
                    , query = model.query
                    }
                    model.article
            ]
        ]
