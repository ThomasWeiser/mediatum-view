module UI exposing
    ( Context, Return(..), Model, Msg
    , init, needs, update, updateOnChangedPresentation, updateOnChangedRoute, view
    )

{-| Top-level module for all visible user-interface components.

@docs Context, Return, Model, Msg
@docs init, needs, update, updateOnChangedPresentation, updateOnChangedRoute, view

-}

import Cache exposing (Cache)
import Config
import Entities.Document exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Types.Aspect exposing (Aspect)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Needs
import Types.Presentation exposing (Presentation(..))
import Types.Route as Route exposing (Route)
import UI.Article
import UI.Controls
import UI.Facets
import UI.Icons
import UI.Tree


{-| Context data provided by the parent module [`App`](App). Used by several functions here.
-}
type alias Context =
    { cache : Cache
    , route : Route
    , presentation : Presentation
    }


{-| Return value of the [`update`](#update) function.

Mainly used to report navigational requests.

-}
type Return
    = NoReturn
    | Navigate Navigation
    | UpdateCacheWithModifiedDocument Document


{-| The model comprises the models of the sub-components.

In our app architecture the UI model contains as little state as possible.

Most navigational state is represented in the route.
All content data is represented in the cache.
Only temporary or subordinate interaction state is represented in the UI model.

-}
type alias Model =
    { tree : UI.Tree.Model
    , facets : UI.Facets.Model
    , controls : UI.Controls.Model
    , article : UI.Article.Model
    , facetAspects : List Aspect
    }


{-| Standard message type, wrapping the messages of the sub-components.
-}
type Msg
    = TreeMsg UI.Tree.Msg
    | FacetsMsg UI.Facets.Msg
    | ControlsMsg UI.Controls.Msg
    | ArticleMsg UI.Article.Msg


{-| Initial model
-}
init : Model
init =
    { tree = UI.Tree.initialModel
    , facets = UI.Facets.initialModel Config.validFacetAspects
    , controls = UI.Controls.initialModel
    , article = UI.Article.initialModel (GenericPresentation Nothing)
    , facetAspects = Config.validFacetAspects
    }


{-| Report the current needs of the UI, gathering the needs of the sub-components.
-}
needs : Context -> Model -> Cache.Needs
needs context model =
    Types.Needs.batch
        [ Types.Needs.atomic Cache.NeedRootFolderIds
        , UI.Tree.needs
            { cache = context.cache
            , presentation = context.presentation
            }
            model.tree
        , UI.Article.needs model.facetAspects context.presentation
        ]


{-| Update the Controls and the Tree to adapt to a changed route.
-}
updateOnChangedRoute : Context -> Model -> Model
updateOnChangedRoute context model =
    { model
        | controls = UI.Controls.updateFromRoute context.route model.controls
        , tree = UI.Tree.expandPresentationFolder model.tree
    }


{-| Update the Article to adapt to a changed presentation.
-}
updateOnChangedPresentation : Presentation -> Model -> Model
updateOnChangedPresentation presentation model =
    { model
        | article =
            UI.Article.initialModel presentation
    }


{-| Standard update function
-}
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

        FacetsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    UI.Facets.update
                        { cache = context.cache
                        , presentation = context.presentation
                        , facetAspects = model.facetAspects
                        }
                        subMsg
                        model.facets
            in
            ( { model
                | facets = subModel
                , facetAspects =
                    case subReturn of
                        UI.Facets.ChangedFacetAspects newFacetAspects ->
                            newFacetAspects

                        _ ->
                            model.facetAspects
              }
            , Cmd.map FacetsMsg subCmd
            , case subReturn of
                UI.Facets.Navigate navigation ->
                    Navigate navigation

                _ ->
                    NoReturn
            )

        ControlsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    UI.Controls.update
                        { route = context.route
                        , cache = context.cache
                        , presentation = context.presentation
                        }
                        subMsg
                        model.controls
            in
            ( { model
                | controls = subModel
              }
            , Cmd.map ControlsMsg subCmd
            , case subReturn of
                UI.Controls.Navigate navigation ->
                    Navigate navigation

                _ ->
                    NoReturn
            )

        ArticleMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    UI.Article.update
                        { cache = context.cache
                        , route = context.route
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


{-| Put together the views of the sub-components into a Html page body.
-}
view : Context -> Model -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "page-container" ]
        [ UI.Icons.definitions
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
                        , Html.Attributes.src "/logo_tum.png"
                        , Html.Attributes.style "float" "right"
                        ]
                        []
                    ]
                ]
            , UI.Controls.view
                { route = context.route
                , cache = context.cache
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
                            , route = context.route
                            , presentation = context.presentation
                            }
                        )
                , Html.map FacetsMsg <|
                    UI.Facets.view
                        { cache = context.cache
                        , presentation = context.presentation
                        , facetAspects = model.facetAspects
                        }
                        model.facets
                ]
            , Html.map ArticleMsg <|
                UI.Article.view
                    { cache = context.cache
                    , route = context.route
                    , presentation = context.presentation
                    }
                    model.article
            ]
        ]
