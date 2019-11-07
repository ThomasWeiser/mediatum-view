module UI exposing
    ( Context
    , Model
    , Msg
    , Return(..)
    , adjust
    , init
    , needs
    , update
    , updateOnChangedRoute
    , view
    )

import Article
import Controls
import Data.Cache as Cache
import Data.Types exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Navigation exposing (Navigation)
import Presentation exposing (Presentation(..))
import Route exposing (Route)
import Tree


type alias Context =
    { cache : Cache.Model
    , route : Route
    , presentation : Presentation
    }


type Return
    = NoReturn
    | Navigate Navigation
    | UpdateCacheWithModifiedDocument Document


type alias Model =
    { tree : Tree.Model
    , controls : Controls.Model
    , article : Article.Model
    }


type Msg
    = TreeMsg Tree.Msg
    | ControlsMsg Controls.Msg
    | ArticleMsg Article.Msg


init : Model
init =
    { tree = Tree.initialModel
    , controls = Controls.initialModel Route.home
    , article =
        Article.initialModel
            { cache = Cache.initialModel
            , presentation = GenericPresentation Nothing
            }
    }


needs : Context -> Model -> Cache.Needs
needs context model =
    Cache.NeedAnd
        (Tree.needs
            { cache = context.cache
            , presentation = context.presentation
            }
            model.tree
        )
        (Article.needs
            { cache = context.cache
            , presentation = context.presentation
            }
        )


updateOnChangedRoute : Context -> Model -> Model
updateOnChangedRoute context model =
    { model
        | controls = Controls.initialModel context.route
        , tree = Tree.expandPresentationFolder model.tree
    }


adjust : Context -> Model -> Model
adjust context model =
    { model
        | article =
            Article.initialModel
                { cache = context.cache
                , presentation = context.presentation
                }
    }


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        TreeMsg subMsg ->
            let
                ( subModel, subReturn ) =
                    Tree.update
                        { cache = context.cache
                        , presentation = context.presentation
                        }
                        subMsg
                        model.tree
            in
            ( { model | tree = subModel }
            , Cmd.none
            , case subReturn of
                Tree.NoReturn ->
                    NoReturn

                Tree.UserSelection selectedFolder ->
                    Navigate (Navigation.ShowListingWithFolder selectedFolder)
            )

        ControlsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Controls.update
                        { route = context.route
                        , presentation = context.presentation
                        }
                        subMsg
                        model.controls
            in
            ( { model | controls = subModel }
            , Cmd.map ControlsMsg subCmd
            , case subReturn of
                Controls.NoReturn ->
                    NoReturn

                Controls.Navigate navigation ->
                    Navigate navigation
            )

        ArticleMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Article.update
                        { cache = context.cache
                        , presentation = context.presentation
                        }
                        subMsg
                        model.article
            in
            ( { model | article = subModel }
            , Cmd.map ArticleMsg subCmd
            , case subReturn of
                Article.NoReturn ->
                    NoReturn

                Article.Navigate navigation ->
                    Navigate navigation

                Article.UpdateCacheWithModifiedDocument document ->
                    UpdateCacheWithModifiedDocument document
            )


view : Context -> Model -> Html Msg
view context model =
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
                { route = context.route
                , presentation = context.presentation
                }
                model.controls
                |> Html.map ControlsMsg
            ]
        , Html.main_ []
            [ Html.aside []
                [ Html.map TreeMsg <|
                    Tree.view
                        { cache = context.cache
                        , presentation = context.presentation
                        }
                        model.tree
                        (Article.folderCountsForQuery
                            { cache = context.cache
                            , presentation = context.presentation
                            }
                        )
                ]
            , Html.map ArticleMsg <|
                Article.view
                    model.tree
                    { cache = context.cache
                    , presentation = context.presentation
                    }
                    model.article
            ]
        ]
