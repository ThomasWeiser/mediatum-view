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
import Data.Types exposing (FolderCounts)
import Dict
import GenericNode exposing (GenericNode)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Query exposing (Query)
import Query.Filters
import Route exposing (Route)
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
    , folderCounts : FolderCounts
    , article : Article.Model
    }


type Msg
    = NoOp
    | QueryGenericNode Int
    | GenericNodeQueryResponse (Api.Response GenericNode)
    | CacheMsg Cache.Msg
    | TreeMsg Tree.Msg
    | ControlsMsg Controls.Msg
    | ArticleMsg Article.Msg


init : Route -> ( Model, Cmd Msg )
init route =
    let
        model1 =
            { route = Route.Invalid "to be initialized"
            , cache = Cache.initialModel
            , query = Query.emptyQuery
            , tree = Tree.initialModel
            , controls = Controls.init ()
            , folderCounts = Dict.empty
            , article = Article.initialModelEmpty
            }

        ( cacheModel, cacheCmd ) =
            Cache.requestNeeds
                (needs model1)
                model1.cache

        ( model2, cmd2 ) =
            ( { model1 | cache = cacheModel }
            , Cmd.map CacheMsg cacheCmd
            )

        ( model3, cmd3 ) =
            changeRouteTo route model2
    in
    ( model3
    , Cmd.batch [ cmd2, cmd3 ]
    )


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
    ([ Cache.NeedRootFolderIds ]
        ++ Tree.needs { cache = model.cache } model.tree
    )
        |> Debug.log "App needs"


update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    let
        ( model1, cmd1 ) =
            updateWithoutReturn
                msg
                model

        ( cacheModel, cacheCmd ) =
            Cache.requestNeeds
                (needs model1)
                model1.cache

        ( model2, cmd2 ) =
            ( { model1 | cache = cacheModel }
            , Cmd.map CacheMsg cacheCmd
            )
    in
    ( model2
    , Cmd.batch [ cmd1, cmd2 ]
    , if model2.route /= model.route then
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
                            }
                        )
                        { model
                            | tree =
                                Tree.selectFolder
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

        CacheMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Cache.update subMsg model.cache
            in
            ( { model | cache = subModel }
            , Cmd.map CacheMsg subCmd
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
                {-
                   Tree.GotRootFolders rootFolders ->
                       case ( model.route, List.head rootFolders ) of
                           ( Route.Home, Just rootFolderToBeQueried ) ->
                               startQuery
                                   (Query.setFolder
                                       rootFolderToBeQueried
                                       model1.query
                                   )
                                   model1

                           _ ->
                               ( { model1
                                   | query =
                                       Query.stopgapFolder
                                           (List.head rootFolders)
                                           model1.query
                                 }
                               , Cmd.none
                               )
                -}
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
                    ( model1, Cmd.none )

                Article.FolderCounts folderCounts1 ->
                    ( { model1 | folderCounts = folderCounts1 }
                    , Cmd.none
                    )

                Article.MapQuery queryMapping ->
                    startQuery (queryMapping model1.query) model1
            )
                |> Cmd.Extra.addCmd (Cmd.map ArticleMsg subCmd)


startQuery : Query -> Model -> ( Model, Cmd Msg )
startQuery query model =
    let
        ( articleModel, articleCmd ) =
            Article.initWithQuery query
    in
    ( { model
        | route = Query.toRoute query
        , query = Debug.log "startQuery" query
        , article = articleModel
        , folderCounts = Dict.empty
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
                        model.folderCounts
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
