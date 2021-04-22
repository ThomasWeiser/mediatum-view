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
import Cache.Derive
import Constants
import Entities.PageSequence as PageSequence exposing (PageSequence)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import Mediatum.Object.FacetValue exposing (count)
import RemoteData exposing (RemoteData)
import String.Format
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
import Utils.List


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
    | LoadMore


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

        LoadMore ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.SetLimit
                    (Constants.incrementLimitOnLoadMore context.limit)
                )
            )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.div []
        [ viewHeader context
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


type alias Linkage =
    { selectionDocumentCount : Maybe Int
    , listingDocumentCount : Int
    , listingIsComplete : Bool
    , canLoadMore : Bool
    , currentNumber : Maybe Int
    , firstId : Maybe DocumentId
    , prevId : Maybe DocumentId
    , nextId : Maybe DocumentId
    }


viewHeader : Context -> Html Msg
viewHeader context =
    let
        linkage =
            getLinkage context
    in
    Html.div []
        [ viewNavigationButtons context linkage
        , Html.text (resultNumberText context linkage)
        ]


resultNumberText : Context -> Linkage -> String
resultNumberText context linkage =
    let
        count =
            linkage.selectionDocumentCount
                |> Maybe.withDefault linkage.listingDocumentCount

        orMore =
            (linkage.selectionDocumentCount == Nothing)
                && not linkage.listingIsComplete
    in
    case linkage.currentNumber of
        Just knownCurrentNumber ->
            Localization.string context.config
                (if orMore then
                    if count == 0 then
                        { en = "Result {{}}"
                        , de = "Resultat {{}}"
                        }

                    else
                        { en = "Result {{}} of at least {{}}"
                        , de = "Resultat {{}} von mindestens {{}}"
                        }

                 else
                    { en = "Result {{}} of {{}}"
                    , de = "Resultat {{}} von {{}}"
                    }
                )
                |> String.Format.value (String.fromInt knownCurrentNumber)
                |> String.Format.value (String.fromInt count)

        Nothing ->
            Localization.string context.config
                (if linkage.listingIsComplete then
                    { en = "This document does not appear in the {{}} results."
                    , de = "Dokument nicht in den {{}} Resultaten vorhanden"
                    }

                 else if count == 0 then
                    { en = "Document not found yet"
                    , de = "Dokument bisher nicht gefunden"
                    }

                 else
                    { en = "Document not found in the first {{}} results"
                    , de = "Dokument nicht in den ersten {{}} Resultaten gefunden"
                    }
                )
                |> String.Format.value (String.fromInt linkage.listingDocumentCount)


viewNavigationButtons : Context -> Linkage -> Html Msg
viewNavigationButtons context linkage =
    let
        button maybeMsg =
            Html.button
                (case maybeMsg of
                    Nothing ->
                        [ Html.Attributes.type_ "button"
                        , Html.Attributes.disabled True
                        ]

                    Just msg ->
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick msg
                        ]
                )

        buttonNavigationToDocumentId maybeId loadMore =
            button
                (maybeId
                    |> Maybe.map
                        (\id ->
                            ReturnNavigation
                                (Navigation.ListOfNavigations
                                    ([ Navigation.ShowDocument context.selection.scope id ]
                                        |> Utils.List.consIf loadMore
                                            (Navigation.SetLimit
                                                (Constants.incrementLimitOnLoadMore context.limit)
                                            )
                                    )
                                )
                        )
                )
    in
    Html.div [] <|
        [ buttonNavigationToDocumentId
            linkage.firstId
            False
            [ Localization.text context.config
                { en = "First Result"
                , de = "erstes Resultat der Liste"
                }
            ]
        ]
            ++ (case linkage.currentNumber of
                    Nothing ->
                        [ button
                            (linkage.canLoadMore |> Utils.ifElse (Just LoadMore) Nothing)
                            [ Localization.text context.config
                                { en = "Load More Results"
                                , de = "weitere Ergebnisse laden"
                                }
                            ]
                        ]

                    Just currentNumber ->
                        [ buttonNavigationToDocumentId
                            linkage.prevId
                            False
                            [ Localization.text context.config
                                { en = "Previous Result"
                                , de = "vorheriges Resultat"
                                }
                            ]
                        , buttonNavigationToDocumentId
                            linkage.nextId
                            (currentNumber == context.limit)
                            [ Localization.text context.config
                                { en = "Next Result"
                                , de = "nächstes Resultat"
                                }
                            ]
                        ]
               )


getLinkage : Context -> Linkage
getLinkage context =
    let
        pageSequence =
            Cache.getDocumentsPages
                context.cache
                ( Config.getMaskName MasksConfig.MaskForListing context.config
                , context.selection
                )

        presentationSegments =
            pageSequence
                |> PageSequence.presentationSegmentsAll

        selectionDocumentCount =
            Cache.Derive.getDocumentCount context.cache context.selection
                |> RemoteData.toMaybe

        ( listingDocumentCount, listingIsComplete ) =
            PageSequence.extent pageSequence

        canLoadMore =
            PageSequence.canLoadMore context.limit pageSequence

        first =
            PageSequence.firstDocument presentationSegments
                |> Maybe.map (.document >> .id)
                |> Maybe.andThen
                    (\firstId ->
                        if firstId == context.documentIdFromSearch.id then
                            Nothing

                        else
                            Just firstId
                    )

        adjacent =
            presentationSegments
                |> PageSequence.findAdjacentDocuments context.documentIdFromSearch.id
                |> Maybe.Extra.unwrap
                    { current = Nothing, prev = Nothing, next = Nothing }
                    (\( maybePrevDocumentResult, thisDocumentResult, maybeNextDocumentResult ) ->
                        { current = Just thisDocumentResult.number
                        , prev = maybePrevDocumentResult |> Maybe.map (.document >> .id)
                        , next = maybeNextDocumentResult |> Maybe.map (.document >> .id)
                        }
                    )
    in
    { selectionDocumentCount = selectionDocumentCount
    , listingDocumentCount = listingDocumentCount
    , listingIsComplete = listingIsComplete
    , canLoadMore = canLoadMore
    , currentNumber = adjacent.current
    , firstId = first
    , prevId = adjacent.prev
    , nextId = adjacent.next
    }
