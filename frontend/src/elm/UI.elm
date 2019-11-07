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

import Data.Cache as Cache
import Data.Types exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Navigation exposing (Navigation)
import Presentation exposing (Presentation(..))
import Route exposing (Route)
import UI.Article
import UI.Controls
import UI.Tree


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
    { tree : UI.Tree.Model
    , controls : UI.Controls.Model
    , article : UI.Article.Model
    }


type Msg
    = TreeMsg UI.Tree.Msg
    | ControlsMsg UI.Controls.Msg
    | ArticleMsg UI.Article.Msg


init : Model
init =
    { tree = UI.Tree.initialModel
    , controls = UI.Controls.initialModel Route.home
    , article =
        UI.Article.initialModel
            { cache = Cache.initialModel
            , presentation = GenericPresentation Nothing
            }
    }


needs : Context -> Model -> Cache.Needs
needs context model =
    Cache.NeedAnd
        (UI.Tree.needs
            { cache = context.cache
            , presentation = context.presentation
            }
            model.tree
        )
        (UI.Article.needs
            { cache = context.cache
            , presentation = context.presentation
            }
        )


updateOnChangedRoute : Context -> Model -> Model
updateOnChangedRoute context model =
    { model
        | controls = UI.Controls.initialModel context.route
        , tree = UI.Tree.expandPresentationFolder model.tree
    }


adjust : Context -> Model -> Model
adjust context model =
    { model
        | article =
            UI.Article.initialModel
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
                    UI.Tree.update
                        { cache = context.cache
                        , presentation = context.presentation
                        }
                        subMsg
                        model.tree
            in
            ( { model | tree = subModel }
            , Cmd.none
            , case subReturn of
                UI.Tree.NoReturn ->
                    NoReturn

                UI.Tree.UserSelection selectedFolder ->
                    Navigate (Navigation.ShowListingWithFolder selectedFolder)
            )

        ControlsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    UI.Controls.update
                        { route = context.route
                        , presentation = context.presentation
                        }
                        subMsg
                        model.controls
            in
            ( { model | controls = subModel }
            , Cmd.map ControlsMsg subCmd
            , case subReturn of
                UI.Controls.NoReturn ->
                    NoReturn

                UI.Controls.Navigate navigation ->
                    Navigate navigation
            )

        ArticleMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    UI.Article.update
                        { cache = context.cache
                        , presentation = context.presentation
                        }
                        subMsg
                        model.article
            in
            ( { model | article = subModel }
            , Cmd.map ArticleMsg subCmd
            , case subReturn of
                UI.Article.NoReturn ->
                    NoReturn

                UI.Article.Navigate navigation ->
                    Navigate navigation

                UI.Article.UpdateCacheWithModifiedDocument document ->
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
                        , Html.Events.onClick (ControlsMsg UI.Controls.submitExampleQuery)
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
            , UI.Controls.view
                { route = context.route
                , presentation = context.presentation
                }
                model.controls
                |> Html.map ControlsMsg
            ]
        , Html.main_ []
            [ Html.aside []
                [ Html.map TreeMsg <|
                    UI.Tree.view
                        { cache = context.cache
                        , presentation = context.presentation
                        }
                        model.tree
                        (UI.Article.folderCountsForQuery
                            { cache = context.cache
                            , presentation = context.presentation
                            }
                        )
                ]
            , Html.map ArticleMsg <|
                UI.Article.view
                    model.tree
                    { cache = context.cache
                    , presentation = context.presentation
                    }
                    model.article
            ]
        ]
