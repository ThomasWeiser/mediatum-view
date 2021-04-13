module UI.Article.Iterator exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs initialModel
@docs update
@docs view

-}

import Cache exposing (Cache)
import Entities.PageSequence as PageSequence exposing (PageSequence)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import Types exposing (DocumentIdFromSearch)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.Id exposing (DocumentId)
import Types.Localization as Localization
import Types.Navigation as Navigation exposing (Navigation)
import Types.Route exposing (Route)
import Types.Selection exposing (Selection)
import UI.Article.Details as Details
import UI.Article.Listing as Listing
import Utils


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , route : Route
    , selection : Selection
    , limit : Int
    , documentIdFromSearch : DocumentIdFromSearch
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation


{-| -}
type alias Model =
    { listing : Listing.Model
    , details : Details.Model
    }


{-| -}
type Msg
    = ListingMsg Listing.Msg
    | DetailsMsg Details.Msg
    | ReturnNavigation Navigation


{-| -}
initialModel : Model
initialModel =
    { listing = Listing.initialModel
    , details = Details.initialModel
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        ListingMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Listing.update
                        { config = context.config
                        , cache = context.cache
                        , route = context.route
                        , selection = context.selection
                        , limit = context.limit
                        }
                        subMsg
                        model.listing
            in
            ( { model | listing = subModel }
            , Cmd.map ListingMsg subCmd
            , case subReturn of
                Listing.NoReturn ->
                    NoReturn

                Listing.Navigate navigation ->
                    Navigate navigation
            )

        DetailsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Details.update
                        { config = context.config
                        , cache = context.cache
                        , route = context.route
                        , documentIdFromSearch = context.documentIdFromSearch
                        }
                        subMsg
                        model.details
            in
            ( { model | details = subModel }
            , Cmd.map DetailsMsg subCmd
            , NoReturn
            )

        ReturnNavigation navigation ->
            ( model
            , Cmd.none
            , Navigate navigation
            )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.div []
        [ viewNavigationButtons context
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ Html.div
                [ Html.Attributes.style "flex" "1" ]
                [ Listing.view
                    { config = context.config
                    , cache = context.cache
                    , route = context.route
                    , selection = context.selection
                    , limit = context.limit
                    }
                    model.listing
                    |> Html.map ListingMsg
                ]
            , Html.div
                [ Html.Attributes.style "flex" "1" ]
                [ Details.view
                    { config = context.config
                    , cache = context.cache
                    , route = context.route
                    , documentIdFromSearch = context.documentIdFromSearch
                    }
                    model.details
                    |> Html.map DetailsMsg
                ]
            ]
        ]


viewNavigationButtons : Context -> Html Msg
viewNavigationButtons context =
    let
        ( maybePrevDocumentId, maybeNextDocumentId ) =
            adjacentDocumentIds context
    in
    Html.div []
        [ Html.button
            (case maybePrevDocumentId of
                Nothing ->
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.disabled True
                    ]

                Just prevDocumentId ->
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick
                        (ReturnNavigation
                            (Navigation.ShowDocument context.selection.scope prevDocumentId)
                        )
                    ]
            )
            [ Localization.text context.config
                { en = "Previous Result"
                , de = "vorheriges Resultat"
                }
            ]
        , Html.button
            (case maybeNextDocumentId of
                Nothing ->
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.disabled True
                    ]

                Just nextDocumentId ->
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick
                        (ReturnNavigation
                            (Navigation.ShowDocument context.selection.scope nextDocumentId)
                        )
                    ]
            )
            [ Localization.text context.config
                { en = "Next Result"
                , de = "nächstes Resultat"
                }
            ]
        ]


adjacentDocumentIds : Context -> ( Maybe DocumentId, Maybe DocumentId )
adjacentDocumentIds context =
    Cache.getDocumentsPages
        context.cache
        ( Config.getMaskName MasksConfig.MaskForListing context.config
        , context.selection
        )
        |> PageSequence.presentationSegments context.limit
        |> PageSequence.findAdjacentDocuments context.documentIdFromSearch.id
        |> Maybe.Extra.unwrap
            ( Nothing, Nothing )
            (\( maybePrevDocumentResult, thisDocumentResult, maybeNextDocumentResult ) ->
                ( maybePrevDocumentResult |> Maybe.map (.document >> .id)
                , maybeNextDocumentResult |> Maybe.map (.document >> .id)
                )
            )
