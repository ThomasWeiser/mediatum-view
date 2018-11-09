module App exposing
    ( Model
    , Msg
    , changeRouteTo
    , init
    , update
    , view
    )

import Api
import Article
import Cmd.Extra
import Controls
import Dict
import Folder exposing (FolderCounts)
import GenericNode exposing (GenericNode)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Maybe.Extra
import Query exposing (Query)
import Query.Filters
import Route exposing (Route)
import Tree
import Utils


type alias Model =
    { query : Query
    , tree : Tree.Model
    , controls : Controls.Model
    , folderCounts : FolderCounts
    , article : Article.Model
    , testingOnly_nodeId : String
    }


type Msg
    = NoOp
    | QueryGenericNode Int
    | GenericNodeQueryResponse (Api.Response GenericNode)
    | TreeMsg Tree.Msg
    | ControlsMsg Controls.Msg
    | ArticleMsg Article.Msg
    | Set_testingOnly_nodeId String


init : Route -> ( Model, Cmd Msg )
init route =
    let
        ( treeModel, treeCmd ) =
            Tree.init

        controlsModel =
            Controls.init ()

        ( articleModel, articleCmd ) =
            Article.initEmpty ()

        model1 =
            { query = Query.emptyQuery
            , tree = treeModel
            , controls = controlsModel
            , folderCounts = Dict.empty
            , article = articleModel
            , testingOnly_nodeId = ""
            }

        ( model2, cmd2 ) =
            changeRouteTo route model1
    in
    ( model2
    , Cmd.batch
        [ Cmd.map TreeMsg treeCmd
        , Cmd.map ArticleMsg articleCmd
        , cmd2
        ]
    )


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Route.Home ->
            ( model, Cmd.none )

        Route.NodeId nodeId ->
            update (QueryGenericNode nodeId) model

        Route.Invalid errorMsg ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        QueryGenericNode nodeId ->
            ( model
            , Api.makeQueryRequest
                GenericNodeQueryResponse
                (Api.queryGenericNode nodeId)
            )

        GenericNodeQueryResponse (Err err) ->
            let
                -- TODO
                _ =
                    Debug.log "GenericNodeQueryResponse"
            in
            ( model, Cmd.none )

        GenericNodeQueryResponse (Ok genericNode) ->
            case genericNode of
                GenericNode.IsFolder lineage ->
                    let
                        ( treeModel, treeCmd ) =
                            Tree.openLineage lineage model.tree

                        model1 =
                            { model | tree = treeModel }
                    in
                    (case Tree.selectedFolder treeModel of
                        Just selectedFolder ->
                            startQuery
                                (Query.OnFolder
                                    { folder = selectedFolder
                                    , filters = Query.Filters.none
                                    }
                                )
                                model1

                        Nothing ->
                            ( model1, Cmd.none )
                    )
                        |> Cmd.Extra.addCmd (Cmd.map TreeMsg treeCmd)

                GenericNode.IsDocument document ->
                    -- Currently we fetch the document once again here,
                    -- which is not a big deal anyway.
                    -- Will decide later what we really want here.
                    startQuery
                        (Query.OnDetails
                            { folder = Query.getFolder model.query
                            , documentId = document.id
                            }
                        )
                        model

                GenericNode.IsNeither ->
                    -- TODO
                    ( model, Cmd.none )

        TreeMsg subMsg ->
            let
                ( subModel, subCmd, changedSelection ) =
                    Tree.update subMsg model.tree

                model1 =
                    { model | tree = subModel }

                maybeSelectedFolder =
                    Tree.selectedFolder model1.tree
            in
            (case ( changedSelection, maybeSelectedFolder ) of
                ( True, Just selectedFolder ) ->
                    startQuery
                        (Query.setFolder
                            selectedFolder
                            model1.query
                        )
                        model1

                _ ->
                    ( model1, Cmd.none )
            )
                |> Cmd.Extra.addCmd (Cmd.map TreeMsg subCmd)

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
                        { query = model.query }
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

        Set_testingOnly_nodeId nodeId ->
            ( { model | testingOnly_nodeId = nodeId }
            , Cmd.none
            )


startQuery : Query -> Model -> ( Model, Cmd Msg )
startQuery query model =
    let
        ( articleModel, articleCmd ) =
            Article.initWithQuery query
    in
    ( { model
        | query = Debug.log "startQuery" query
        , article = articleModel
        , folderCounts = Dict.empty
      }
    , Cmd.map ArticleMsg articleCmd
    )


andThenUpdate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenUpdate msg ( model1, cmd1 ) =
    let
        ( model2, cmd2 ) =
            update msg model1
    in
    ( model2, Cmd.batch [ cmd1, cmd2 ] )


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "page-container" ]
        [ Icons.definitions
        , Html.header []
            [ Html.h2 []
                [ Html.div []
                    [ Html.span
                        [ Html.Attributes.class "title" ]
                        [ Html.text "mediaTUM view" ]
                    , Html.span
                        [ Html.Attributes.class "subtitle"
                        , Html.Attributes.title "You may click here to start an example query."
                        , Html.Events.onClick (ControlsMsg Controls.submitExampleQuery)
                        ]
                        [ Html.text "WIP 2018-10-29" ]
                    , Html.img
                        [ Html.Attributes.alt "TUM Logo"
                        , Html.Attributes.src "logo_tum.png"
                        , Html.Attributes.style "float" "right"
                        ]
                        []
                    ]
                ]
            , -- Dev-only testing input
              Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.placeholder "node id"
                , Utils.onChange Set_testingOnly_nodeId
                ]
                []
            , Html.a
                [ Html.Attributes.href model.testingOnly_nodeId ]
                [ Html.text "Link to this node id" ]
            , Controls.view { query = model.query } model.controls
                |> Html.map ControlsMsg
            ]
        , Html.main_ []
            [ Html.aside []
                [ Html.map TreeMsg <| Tree.view model.tree model.folderCounts
                ]
            , Html.map ArticleMsg <|
                Article.view
                    model.tree
                    { query = model.query }
                    model.article
            ]
        ]
