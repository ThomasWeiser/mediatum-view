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
import Entities.Document
import Entities.FolderCounts exposing (FolderCounts)
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import RemoteData
import Types.Aspect exposing (Aspect)
import Types.Config as Config exposing (Config)
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
import UI.Icons exposing (search)
import UI.Widgets.Breadcrumbs
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
    = GenericMsg UI.Article.Generic.Msg
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
needs : Config -> List Aspect -> Presentation -> Cache.Needs
needs config facetAspects presentation =
    case presentation of
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
                                        (Config.getMaskName MasksConfig.MaskForDetails config)
                                    )
                    in
                    [ Cache.NeedGenericNode
                        (Config.getMaskName MasksConfig.MaskForDetails config)
                        nodeIdOne
                    ]
                        |> Maybe.Extra.cons maybeNeedTwo
                        |> List.map Types.Needs.atomic
                        |> Types.Needs.batch

        DocumentPresentation maybeFolderId documentIdFromSearch ->
            Cache.NeedDocumentFromSearch
                (Config.getMaskName MasksConfig.MaskForDetails config)
                documentIdFromSearch
                |> Types.Needs.atomic

        CollectionPresentation folderId ->
            Types.Needs.none

        ListingPresentation selection limit ->
            Types.Needs.sequence
                (Types.Needs.atomic <|
                    Cache.NeedDocumentsPage
                        (Config.getMaskName MasksConfig.MaskForListing config)
                        selection
                        limit
                )
                (Types.Needs.batch
                    [ Types.Needs.atomic <| Cache.NeedFolderCounts selection
                    , Types.Needs.atomic <| Cache.NeedFacets selection facetAspects
                    ]
                )

        IteratorPresentation selection limit documentIdFromSearch ->
            let
                maybeIndexOfDocument =
                    Cache.getDocumentsPages
                        context.cache
                        ( Config.getMaskName MasksConfig.MaskForListing context.config
                        , context.selection
                        )

                presentationSegments =
                    pageSequence
                        |> PageSequence.presentationSegments context.limit
                            PageSequence.findIndex
            in
            Types.Needs.batch
                [ needs config facetAspects (DocumentPresentation (Just selection.scope) documentIdFromSearch)
                , needs config facetAspects (ListingPresentation selection limit)
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
        [ viewBreadcrumbs
            context
            (Presentation.getFolderId context.cache context.presentation)
        , viewContent context model
        ]


viewBreadcrumbs : Context -> Maybe FolderId -> Html msg
viewBreadcrumbs context maybeFolderId =
    maybeFolderId
        |> Maybe.andThen
            (Cache.Derive.getPath context.config context.cache
                >> RemoteData.toMaybe
            )
        |> UI.Widgets.Breadcrumbs.view context


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
