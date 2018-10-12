module Main exposing (main)

import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Select
import Folder exposing (FolderCounts)
import Tree
import Article
import Article.Fts exposing (SearchType)
import Utils


type alias Model =
    { searchType : SearchType
    , searchString : String
    , tree : Tree.Model
    , folderCounts : FolderCounts
    , article : Article.Model
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Msg
    = SearchString String
    | SetSearchType SearchType
    | Submit
    | TreeMsg Tree.Msg
    | ArticleMsg Article.Msg


init : ( Model, Cmd Msg )
init =
    let
        initialSearchType =
            Article.Fts.FtsSearch
                Article.Fts.SearchAttributes
                Article.Fts.English

        ( treeModel, treeCmd ) =
            Tree.init

        ( articleModel, articleCmd ) =
            Article.initEmpty ()

        model =
            { searchType = initialSearchType
            , searchString = ""
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
        SearchString str ->
            ( { model | searchString = str }
            , Cmd.none
            )

        SetSearchType searchType ->
            ( { model | searchType = searchType }
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
                    Article.update subMsg model.article

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
                        ( articleModel, articleCmd ) =
                            Article.initCollectionOrSearch
                                selectedFolder
                                model.searchType
                                model.searchString
                    in
                        ( { model
                            | article = articleModel
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
                    Article.view model.tree model.article
                ]
            ]
        ]


viewSearchControls : Model -> Html Msg
viewSearchControls model =
    Html.form
        [ Html.Attributes.class "search-bar"
        , Html.Events.onSubmit Submit
        ]
        [ Select.fromSelected_
            [ Article.Fts.FtsSearch
                Article.Fts.SearchAttributes
                Article.Fts.English
            , Article.Fts.FtsSearch
                Article.Fts.SearchAttributes
                Article.Fts.German
            , Article.Fts.FtsSearch
                Article.Fts.SearchFulltext
                Article.Fts.English
            , Article.Fts.FtsSearch
                Article.Fts.SearchFulltext
                Article.Fts.German
            ]
            SetSearchType
            toString
            Article.Fts.searchTypeText
            model.searchType
        , Html.input
            [ Html.Attributes.class "search-input"
            , Html.Attributes.type_ "search"
            , Html.Attributes.placeholder "Search ..."
            , Html.Attributes.value model.searchString
            , Html.Events.onInput SearchString
            ]
            []
        , Html.button
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.value "Search"
            ]
            [ Icons.search ]
        ]
