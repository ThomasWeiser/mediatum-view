module UI exposing
    ( Context, Return(..), Model, Msg
    , init, needs, update, updateOnChangedPresentation, updateOnChangedRoute, view
    )

{-| Top-level module for all visible user-interface components.

@docs Context, Return, Model, Msg
@docs init, needs, update, updateOnChangedPresentation, updateOnChangedRoute, view

-}

import Cache exposing (Cache)
import Constants
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Types.AdjustmentToSetup as AdjustmentToSetup exposing (AdjustmentToSetup)
import Types.Config exposing (Config)
import Types.Localization as Localization exposing (Language)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Needs
import Types.Presentation exposing (Presentation(..))
import Types.Route exposing (Route)
import UI.Article
import UI.Controls
import UI.Facets
import UI.Icons
import UI.Tree
import UI.Widgets.LanguageSelect


{-| Context data provided by the parent module [`App`](App). Used by several functions here.
-}
type alias Context =
    { config : Config
    , cache : Cache
    , route : Route
    , presentation : Presentation
    }


{-| Return value of the [`update`](#update) function.

Mainly used to report navigational requests.

-}
type Return
    = NoReturn
    | Navigate Navigation
    | AdjustSetup AdjustmentToSetup


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
    }


{-| Standard message type, wrapping the messages of the sub-components.
-}
type Msg
    = ReturnAdjustmentToSetup AdjustmentToSetup
    | TreeMsg UI.Tree.Msg
    | FacetsMsg UI.Facets.Msg
    | ControlsMsg UI.Controls.Msg
    | ArticleMsg UI.Article.Msg


{-| Initial model
-}
init : Config -> Model
init config =
    { tree = UI.Tree.initialModel
    , facets = UI.Facets.initialModel
    , controls = UI.Controls.initialModel config
    , article = UI.Article.initialModel (GenericPresentation Nothing)
    }


{-| Report the current needs of the UI, gathering the needs of the sub-components.
-}
needs : Context -> Model -> Cache.Needs
needs context model =
    Types.Needs.batch
        [ Types.Needs.atomic (Cache.NeedFolders context.config.toplevelFolderIds)
        , UI.Tree.needs
            { config = context.config
            , cache = context.cache
            , presentation = context.presentation
            }
            model.tree
        , UI.Article.needs context
        ]


{-| Update the Controls and the Tree to adapt to a changed route.
-}
updateOnChangedRoute : Context -> Model -> Model
updateOnChangedRoute context model =
    { model
        | controls = UI.Controls.updateFromRoute context.route model.controls
    }


{-| Update the Article to adapt to a changed presentation.
-}
updateOnChangedPresentation : Context -> Model -> Model
updateOnChangedPresentation context model =
    { model
        | article =
            UI.Article.initialModel context.presentation
        , tree =
            UI.Tree.updateOnPresentationFolderId
                { config = context.config
                , cache = context.cache
                , presentation = context.presentation
                }
                model.tree
    }


{-| Standard update function
-}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        ReturnAdjustmentToSetup adjustment ->
            ( model
            , Cmd.none
            , AdjustSetup adjustment
            )

        TreeMsg subMsg ->
            let
                ( subModel, subReturn ) =
                    UI.Tree.update
                        { config = context.config
                        , cache = context.cache
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
                        { config = context.config
                        , cache = context.cache
                        , presentation = context.presentation
                        }
                        subMsg
                        model.facets
            in
            ( { model
                | facets = subModel
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
                        { config = context.config
                        , route = context.route
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
                        { config = context.config
                        , cache = context.cache
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

                UI.Article.AdjustSetup adjustment ->
                    AdjustSetup adjustment
            )


{-| Put together the views of the sub-components into a Html page body.
-}
view : Context -> Model -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "page-container" ]
        [ UI.Icons.definitions
        , Html.header []
            [ Html.div
                [ Html.Attributes.class "tum-header" ]
                [ Html.div
                    [ Html.Attributes.class "tum-links" ]
                    [ Html.div
                        [ Html.Attributes.style "float" "right" ]
                        [ UI.Widgets.LanguageSelect.view
                            context.config.uiLanguage
                            (\language ->
                                ReturnAdjustmentToSetup (AdjustmentToSetup.UserSelectedUILanguage language)
                            )
                        , Html.a
                            [ Html.Attributes.href "https://www.tum.de/" ]
                            [ Html.img
                                [ Html.Attributes.alt "TUM Logo"
                                , Html.Attributes.src "/logo_tum.png"
                                ]
                                []
                            ]
                        ]
                    , Html.div []
                        [ Html.div []
                            [ Html.a
                                [ Html.Attributes.href "/" ]
                                [ Localization.text context.config
                                    { en = "University Bibliography"
                                    , de = "Hochschulbibliographie"
                                    }
                                ]
                            ]
                        , Html.div []
                            [ Html.a
                                [ Html.Attributes.href "https://www.ub.tum.de/" ]
                                [ Localization.text context.config
                                    { en = "University Library"
                                    , de = "Universitätsbibliothek"
                                    }
                                ]
                            ]
                        , Html.div []
                            [ Html.a
                                [ Html.Attributes.href "https://www.tum.de/" ]
                                [ Localization.text context.config
                                    { en = "Technical University of Munich"
                                    , de = "Technische Universität München"
                                    }
                                ]
                            ]
                        ]
                    ]
                ]
            , UI.Controls.view
                { config = context.config
                , route = context.route
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
                        { config = context.config
                        , cache = context.cache
                        , presentation = context.presentation
                        }
                        model.tree
                        (UI.Article.folderCountsForQuery
                            { config = context.config
                            , cache = context.cache
                            , route = context.route
                            , presentation = context.presentation
                            }
                        )
                , Html.map FacetsMsg <|
                    UI.Facets.view
                        { config = context.config
                        , cache = context.cache
                        , presentation = context.presentation
                        }
                        model.facets
                ]
            , Html.map ArticleMsg <|
                UI.Article.view
                    { config = context.config
                    , cache = context.cache
                    , route = context.route
                    , presentation = context.presentation
                    }
                    model.article
            ]
        , Html.footer [] <|
            let
                item itemSpec en de =
                    Html.a
                        [ Html.Attributes.href (Constants.externalServerUrls.item itemSpec) ]
                        [ Localization.text context.config { en = en, de = de } ]
            in
            -- TODO: Fix the links. We currently cannot show ids like "604993_12"
            [ Html.span
                [ Html.Attributes.class "footer-right" ]
                [ item "604993_13" "Impressum" "Impressum"
                , item "604993_18" "Barrierefreiheit" "Barrierefreiheit"
                , item "604993_9" "Privacy" "Datenschutz"
                , item "604993_12" "Nutzungsrechte" "Nutzungsrechte"
                ]
            , Html.span
                [ Html.Attributes.class "footer-left" ]
                [ item "mailto:mediatum@ub.tum.de" "mediatum@ub.tum.de" "mediatum@ub.tum.de"
                , item "604993_15" "Contact" "Kontakt"
                , item "604993_17" "About mediaTUM" "Über mediaTUM"
                , item "604993_14" "Help" "Hilfe"
                ]
            ]
        ]
