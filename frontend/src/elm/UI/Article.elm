module UI.Article exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , needs
    , folderCountsForQuery
    , update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs initialModel
@docs needs
@docs folderCountsForQuery
@docs update
@docs view

-}

import Cache exposing (Cache)
import Cache.Derive
import Constants
import Entities.FolderCounts exposing (FolderCounts)
import Entities.PageSequence as PageSequence
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import RemoteData
import Types.AdjustmentToSetup as AdjustmentToSetup exposing (AdjustmentToSetup)
import Types.ApiData exposing (ApiData)
import Types.Config as Config exposing (Config)
import Types.Config.FacetAspectConfig as FacetAspect
import Types.Config.MasksConfig as MasksConfig
import Types.Id exposing (FolderId)
import Types.Navigation exposing (Navigation)
import Types.Needs
import Types.Presentation as Presentation exposing (Presentation(..))
import Types.Route exposing (Route)
import UI.Article.Collection
import UI.Article.Details
import UI.Article.Generic
import UI.Article.Iterator
import UI.Article.Listing
import UI.Widgets.ThumbnailSwitch
import Utils


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , route : Route
    , presentation : Presentation
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation
    | AdjustSetup AdjustmentToSetup


{-| -}
type alias Model =
    { content : Content
    }


type Content
    = GenericModel UI.Article.Generic.Model
    | CollectionModel UI.Article.Collection.Model
    | ListingModel UI.Article.Listing.Model
    | DetailsModel UI.Article.Details.Model
    | IteratorModel UI.Article.Iterator.Model


{-| -}
type Msg
    = ReturnAdjustmentToSetup AdjustmentToSetup
    | GenericMsg UI.Article.Generic.Msg
    | CollectionMsg UI.Article.Collection.Msg
    | ListingMsg UI.Article.Listing.Msg
    | DetailsMsg UI.Article.Details.Msg
    | IteratorMsg UI.Article.Iterator.Msg


{-| -}
initialModel : Presentation -> Model
initialModel presentation =
    case presentation of
        GenericPresentation maybeNodeIds ->
            { content = GenericModel UI.Article.Generic.initialModel }

        DocumentPresentation maybeFolderId documentIdFromSearch ->
            { content = DetailsModel UI.Article.Details.initialModel }

        CollectionPresentation folderId ->
            { content = CollectionModel UI.Article.Collection.initialModel }

        ListingPresentation selection limit ->
            { content = ListingModel UI.Article.Listing.initialModel }

        IteratorPresentation selection limit documentIdFromSearch ->
            { content = IteratorModel UI.Article.Iterator.initialModel }


{-| -}
needs : Context -> Cache.Needs
needs context =
    let
        facetAspects =
            FacetAspect.aspects context.config.facetAspects

        needsOfDocumentPresentation maybeFolderId documentIdFromSearch =
            Cache.NeedDocumentFromSearch
                (Config.getMaskName MasksConfig.MaskForDetails context.config)
                documentIdFromSearch
                |> Types.Needs.atomic

        needsOfListingPresentation selection limit =
            Types.Needs.sequence
                (Types.Needs.atomic <|
                    Cache.NeedDocumentsPage
                        (Config.getMaskName MasksConfig.MaskForListing context.config)
                        selection
                        limit
                )
                (Types.Needs.batch
                    [ Types.Needs.atomic <| Cache.NeedFolderCounts selection
                    , Types.Needs.atomic <| Cache.NeedFacets selection facetAspects
                    ]
                )
    in
    case context.presentation of
        GenericPresentation genericParameters ->
            case genericParameters of
                Nothing ->
                    Types.Needs.none

                Just ( nodeIdOne, maybeDocumentIdFromSearch ) ->
                    let
                        maybeNeedTwo =
                            maybeDocumentIdFromSearch
                                |> Maybe.map
                                    (Cache.NeedDocumentFromSearch
                                        (Config.getMaskName MasksConfig.MaskForDetails context.config)
                                    )
                    in
                    [ Cache.NeedGenericNode
                        (Config.getMaskName MasksConfig.MaskForDetails context.config)
                        nodeIdOne
                    ]
                        |> Maybe.Extra.cons maybeNeedTwo
                        |> List.map Types.Needs.atomic
                        |> Types.Needs.batch

        DocumentPresentation maybeFolderId documentIdFromSearch ->
            needsOfDocumentPresentation maybeFolderId documentIdFromSearch

        CollectionPresentation folderId ->
            Types.Needs.none

        ListingPresentation selection limit ->
            needsOfListingPresentation selection limit

        IteratorPresentation selection limit documentIdFromSearch ->
            let
                remoteMaybeIndexOfDocument : ApiData (Maybe Int)
                remoteMaybeIndexOfDocument =
                    Cache.getDocumentsPages
                        context.cache
                        ( Config.getMaskName MasksConfig.MaskForListing context.config
                        , selection
                        )
                        |> PageSequence.presentationSegmentsAll
                        |> PageSequence.findIndex documentIdFromSearch.id

                raisedLimit =
                    case remoteMaybeIndexOfDocument of
                        RemoteData.Success (Just indexOfDocument) ->
                            if indexOfDocument == limit then
                                Constants.incrementLimitOnLoadMore context.config limit

                            else
                                limit

                        _ ->
                            limit
            in
            Types.Needs.batch
                [ needsOfDocumentPresentation (Just selection.scope) documentIdFromSearch
                , Types.Needs.sequence
                    (needsOfListingPresentation selection limit)
                    (if raisedLimit > limit then
                        needsOfListingPresentation selection raisedLimit

                     else
                        Types.Needs.none
                    )
                ]


{-| -}
folderCountsForQuery : Context -> Maybe FolderCounts
folderCountsForQuery context =
    case context.presentation of
        GenericPresentation maybeNodeIds ->
            Nothing

        DocumentPresentation maybeFolderId documentIdFromSearch ->
            Nothing

        CollectionPresentation folderId ->
            Nothing

        ListingPresentation selection limit ->
            Cache.Derive.folderCountsOnPath context.config context.cache selection
                |> Just

        IteratorPresentation selection limit documentIdFromSearch ->
            Cache.Derive.folderCountsOnPath context.config context.cache selection
                |> Just


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case ( msg, model.content, context.presentation ) of
        ( ReturnAdjustmentToSetup adjustment, _, _ ) ->
            ( model
            , Cmd.none
            , AdjustSetup adjustment
            )

        ( GenericMsg subMsg, GenericModel subModel, _ ) ->
            let
                ( subModel1, subCmd ) =
                    UI.Article.Generic.update subMsg subModel
            in
            ( { model | content = GenericModel subModel1 }
            , Cmd.map GenericMsg subCmd
            , NoReturn
            )

        ( CollectionMsg subMsg, CollectionModel subModel, _ ) ->
            let
                ( subModel1, subCmd ) =
                    UI.Article.Collection.update subMsg subModel
            in
            ( { model | content = CollectionModel subModel1 }
            , Cmd.map CollectionMsg subCmd
            , NoReturn
            )

        ( ListingMsg subMsg, ListingModel subModel, ListingPresentation selection limit ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    UI.Article.Listing.update
                        { config = context.config
                        , cache = context.cache
                        , route = context.route
                        , selection = selection
                        , limit = limit
                        }
                        subMsg
                        subModel
            in
            ( { model | content = ListingModel subModel1 }
            , Cmd.map ListingMsg subCmd
            )
                |> Utils.tupleAddThird
                    (case subReturn of
                        UI.Article.Listing.NoReturn ->
                            NoReturn

                        UI.Article.Listing.Navigate navigation ->
                            Navigate navigation
                    )

        ( DetailsMsg subMsg, DetailsModel subModel, DocumentPresentation maybeFolderId documentIdFromSearch ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    UI.Article.Details.update
                        { config = context.config
                        , cache = context.cache
                        , route = context.route
                        , documentIdFromSearch = documentIdFromSearch
                        }
                        subMsg
                        subModel
            in
            ( { model | content = DetailsModel subModel1 }
            , Cmd.map DetailsMsg subCmd
            , case subReturn of
                UI.Article.Details.NoReturn ->
                    NoReturn
            )

        ( IteratorMsg subMsg, IteratorModel subModel, IteratorPresentation selection limit documentIdFromSearch ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    UI.Article.Iterator.update
                        { config = context.config
                        , cache = context.cache
                        , route = context.route
                        , selection = selection
                        , limit = limit
                        , documentIdFromSearch = documentIdFromSearch
                        }
                        subMsg
                        subModel
            in
            ( { model | content = IteratorModel subModel1 }
            , Cmd.map IteratorMsg subCmd
            )
                |> Utils.tupleAddThird
                    (case subReturn of
                        UI.Article.Iterator.NoReturn ->
                            NoReturn

                        UI.Article.Iterator.Navigate navigation ->
                            Navigate navigation
                    )

        _ ->
            -- Message doesn't match model; should never happen.
            -- Or model doesn't match presentation; should never happen.
            ( model, Cmd.none, NoReturn )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.article
        [ Html.Attributes.class "article" ]
        [ viewThumbnailsSwitch context model
        , viewContent context model
        ]


viewThumbnailsSwitch : Context -> Model -> Html Msg
viewThumbnailsSwitch context model =
    let
        showSwitch =
            case ( model.content, context.presentation ) of
                ( GenericModel _, _ ) ->
                    False

                ( CollectionModel _, _ ) ->
                    False

                ( ListingModel _, ListingPresentation selection limit ) ->
                    UI.Article.Listing.hasAtLeastOneDocument
                        { config = context.config
                        , cache = context.cache
                        , route = context.route
                        , selection = selection
                        , limit = limit
                        }

                ( DetailsModel _, _ ) ->
                    True

                ( IteratorModel _, IteratorPresentation selection limit documentIdFromSearch ) ->
                    True

                _ ->
                    -- Model doesn't match query-context; should never happen.
                    False
    in
    if showSwitch then
        Html.span [ Html.Attributes.class "thumbnail-switch" ]
            [ UI.Widgets.ThumbnailSwitch.view
                context.config
                context.config.hideThumbnails
                (ReturnAdjustmentToSetup << AdjustmentToSetup.HideThumbnails)
            ]

    else
        Html.text ""


viewContent : Context -> Model -> Html Msg
viewContent context model =
    case ( model.content, context.presentation ) of
        ( GenericModel subModel, GenericPresentation genericParameters ) ->
            UI.Article.Generic.view
                { config = context.config
                , cache = context.cache
                , genericParameters = genericParameters
                }
                subModel
                |> Html.map GenericMsg

        ( CollectionModel subModel, CollectionPresentation folderId ) ->
            UI.Article.Collection.view
                { config = context.config
                , cache = context.cache
                , folderId = folderId
                }
                subModel
                |> Html.map CollectionMsg

        ( ListingModel subModel, ListingPresentation selection limit ) ->
            UI.Article.Listing.view
                { config = context.config
                , cache = context.cache
                , route = context.route
                , selection = selection
                , limit = limit
                }
                subModel
                |> Html.map ListingMsg

        ( DetailsModel subModel, DocumentPresentation maybeFolderId documentIdFromSearch ) ->
            UI.Article.Details.view
                { config = context.config
                , cache = context.cache
                , route = context.route
                , documentIdFromSearch = documentIdFromSearch
                }
                subModel
                |> Html.map DetailsMsg

        ( IteratorModel subModel, IteratorPresentation selection limit documentIdFromSearch ) ->
            UI.Article.Iterator.view
                { config = context.config
                , cache = context.cache
                , route = context.route
                , selection = selection
                , limit = limit
                , documentIdFromSearch = documentIdFromSearch
                }
                subModel
                |> Html.map IteratorMsg

        _ ->
            -- Model doesn't match query-context; should never happen.
            Html.text ""
