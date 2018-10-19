module Main exposing (main)

import Article
import Browser
import Dict
import Folder exposing (FolderCounts)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Maybe.Extra
import Query exposing (Query)
import Tree
import Utils


type alias Model =
    { searchOptions : Query.FtsOptions
    , searchTerm : String
    , query : Query
    , tree : Tree.Model
    , folderCounts : FolderCounts
    , article : Article.Model
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Msg
    = NoOp
    | SetSearchTerm String
    | SetSearchOptions Query.FtsOptions
    | Submit
    | TreeMsg Tree.Msg
    | ArticleMsg Article.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialSearchType =
            Query.FtsOptions
                Query.SearchFulltext
                Query.English

        ( treeModel, treeCmd ) =
            Tree.init

        ( articleModel, articleCmd ) =
            Article.initEmpty ()

        model =
            { searchOptions = initialSearchType
            , searchTerm = ""
            , query = Query.emptyQuery
            , tree = treeModel
            , folderCounts = Dict.empty
            , article = articleModel
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map TreeMsg treeCmd
        , Cmd.map ArticleMsg articleCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSearchTerm str ->
            ( { model | searchTerm = str }
            , Cmd.none
            )

        SetSearchOptions searchOptions ->
            ( { model | searchOptions = searchOptions }
            , Cmd.none
            )

        TreeMsg subMsg ->
            let
                ( subModel, subCmd, changedSelection ) =
                    Tree.update subMsg model.tree
            in
            ( { model | tree = subModel }
            , Cmd.map TreeMsg subCmd
            )
                |> Utils.when
                    (andThenUpdate Submit)
                    changedSelection

        ArticleMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Article.update
                        { query = model.query }
                        subMsg
                        model.article

                folderCounts =
                    case subReturn of
                        Article.NoReturn ->
                            model.folderCounts

                        Article.FolderCounts folderCounts1 ->
                            folderCounts1
            in
            ( { model
                | folderCounts = folderCounts
                , article = subModel
              }
            , Cmd.map ArticleMsg subCmd
            )

        Submit ->
            case model.tree |> Tree.selectedFolder of
                Just selectedFolder ->
                    let
                        query =
                            if model.searchTerm == "" then
                                if selectedFolder.isCollection then
                                    Query.CollectionQuery
                                        { folder = selectedFolder
                                        }

                                else
                                    Query.DirectoryQuery
                                        { folder = selectedFolder
                                        , filters = Query.exampleFilters
                                        }

                            else
                                Query.FtsQuery
                                    { folder = selectedFolder
                                    , filters = Query.exampleFilters
                                    , options = model.searchOptions
                                    , searchTerm = model.searchTerm
                                    }

                        ( articleModel, articleCmd ) =
                            Article.initWithQuery query
                    in
                    ( { model
                        | query = query
                        , article = articleModel
                        , folderCounts = Dict.empty
                      }
                    , Cmd.map ArticleMsg articleCmd
                    )

                Nothing ->
                    ( model, Cmd.none )


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
                    [ Html.span [ Html.Attributes.class "title" ]
                        [ Html.text "mediaTUM view" ]
                    , Html.span [ Html.Attributes.class "subtitle" ]
                        [ Html.text "WIP 2018-10-09" ]
                    ]
                , Html.div
                    [ Html.Attributes.class "subtitle" ]
                    [ Html.text "PostgreSQL · PostGraphile · GraphQL · Elm" ]
                ]
            , viewSearchControls model
            ]
        , Html.main_ []
            [ Html.aside []
                [ Html.map TreeMsg <| Tree.view model.tree model.folderCounts
                ]
            , Html.article
                [ Html.Attributes.class "article" ]
                [ Html.map ArticleMsg <|
                    Article.view
                        model.tree
                        { query = model.query }
                        model.article
                ]
            ]
        ]


viewSearchControls : Model -> Html Msg
viewSearchControls model =
    Html.form
        [ Html.Attributes.class "search-bar"
        , Html.Events.onSubmit Submit
        ]
        [ Html.select
            [ Html.Events.onInput
                (Query.ftsOptionsFromLabel
                    >> Maybe.Extra.unwrap NoOp SetSearchOptions
                )
            ]
            (List.map
                (\ftsOptions ->
                    Html.option
                        [ Html.Attributes.value
                            (Query.ftsOptionsToLabel ftsOptions)
                        , Html.Attributes.selected
                            (model.searchOptions == ftsOptions)
                        ]
                        [ Html.text
                            (Query.ftsOptionsToLabel ftsOptions)
                        ]
                )
                [ Query.FtsOptions Query.SearchAttributes Query.English
                , Query.FtsOptions Query.SearchAttributes Query.German
                , Query.FtsOptions Query.SearchFulltext Query.English
                , Query.FtsOptions Query.SearchFulltext Query.German
                ]
            )
        , Html.input
            [ Html.Attributes.class "search-input"
            , Html.Attributes.type_ "search"
            , Html.Attributes.placeholder "Search ..."
            , Html.Attributes.value model.searchTerm
            , Html.Events.onInput SetSearchTerm
            ]
            []
        , Html.button
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.value "Search"
            ]
            [ Icons.search ]
        ]
