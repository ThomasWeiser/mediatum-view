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
import Entities.Document exposing (Document)
import Entities.FolderCounts as FolderCounts exposing (FolderCounts)
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import RemoteData
import Types.Id exposing (FolderId)
import Types.Navigation exposing (Navigation)
import Types.Needs
import Types.Presentation as Presentation exposing (Presentation(..))
import Types.Route exposing (Route)
import UI.Article.Collection
import UI.Article.Details
import UI.Article.Generic
import UI.Article.Listing
import UI.Widgets.Breadcrumbs
import Utils


{-| -}
type alias Context =
    { cache : Cache
    , route : Route
    , presentation : Presentation
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation
    | UpdateCacheWithModifiedDocument Document


{-| -}
type alias Model =
    { content : Content
    }


type Content
    = GenericModel UI.Article.Generic.Model
    | CollectionModel UI.Article.Collection.Model
    | ListingModel UI.Article.Listing.Model
    | DetailsModel UI.Article.Details.Model


{-| -}
type Msg
    = GenericMsg UI.Article.Generic.Msg
    | CollectionMsg UI.Article.Collection.Msg
    | ListingMsg UI.Article.Listing.Msg
    | DetailsMsg UI.Article.Details.Msg


{-| -}
initialModel : Presentation -> Model
initialModel presentation =
    case presentation of
        GenericPresentation maybeNodeIds ->
            { content = GenericModel UI.Article.Generic.initialModel }

        DocumentPresentation maybeFolderId documentId ->
            { content = DetailsModel UI.Article.Details.initialModel }

        CollectionPresentation folderId ->
            { content = CollectionModel UI.Article.Collection.initialModel }

        ListingPresentation selection window ->
            { content = ListingModel UI.Article.Listing.initialModel }


{-| -}
needs : List String -> Presentation -> Cache.Needs
needs facetKeys presentation =
    case presentation of
        GenericPresentation maybeNodeIds ->
            case maybeNodeIds of
                Nothing ->
                    Types.Needs.none

                Just ( nodeIdOne, maybeNodeIdTwo ) ->
                    [ nodeIdOne ]
                        |> Maybe.Extra.cons maybeNodeIdTwo
                        |> List.map Cache.NeedGenericNode
                        |> List.map Types.Needs.atomic
                        |> Types.Needs.batch

        DocumentPresentation maybeFolderId documentIdWithSearch ->
            Cache.NeedDocumentWithSearch documentIdWithSearch
                |> Types.Needs.atomic

        CollectionPresentation folderId ->
            Types.Needs.none

        ListingPresentation selection window ->
            Types.Needs.sequence
                (Types.Needs.atomic <| Cache.NeedDocumentsPage selection window)
                (Types.Needs.batch <|
                    (Types.Needs.atomic <| Cache.NeedFolderCounts selection)
                        :: List.map
                            (Types.Needs.atomic << Cache.NeedFacet selection)
                            facetKeys
                )


{-| -}
folderCountsForQuery : Context -> Maybe FolderCounts
folderCountsForQuery context =
    case context.presentation of
        GenericPresentation maybeNodeIds ->
            Nothing

        DocumentPresentation maybeFolderId documentId ->
            Nothing

        CollectionPresentation folderId ->
            Nothing

        ListingPresentation selection window ->
            Cache.get context.cache.folderCounts selection
                |> RemoteData.withDefault FolderCounts.init
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

        ( ListingMsg subMsg, ListingModel subModel, ListingPresentation selection window ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    UI.Article.Listing.update
                        { cache = context.cache
                        , selection = selection
                        , window = window
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

        ( DetailsMsg subMsg, DetailsModel subModel, DocumentPresentation maybeFolderId documentIdWithSearch ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    UI.Article.Details.update
                        { cache = context.cache
                        , route = context.route
                        , documentIdWithSearch = documentIdWithSearch
                        }
                        subMsg
                        subModel
            in
            ( { model | content = DetailsModel subModel1 }
            , Cmd.map DetailsMsg subCmd
            , case subReturn of
                UI.Article.Details.NoReturn ->
                    NoReturn

                UI.Article.Details.UpdateCacheWithModifiedDocument document ->
                    UpdateCacheWithModifiedDocument document
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
            (Cache.Derive.getPath context.cache
                >> RemoteData.toMaybe
            )
        |> UI.Widgets.Breadcrumbs.view context


viewContent : Context -> Model -> Html Msg
viewContent context model =
    case ( model.content, context.presentation ) of
        ( GenericModel subModel, GenericPresentation nodeIds ) ->
            UI.Article.Generic.view
                { cache = context.cache
                , nodeIds = nodeIds
                }
                subModel
                |> Html.map GenericMsg

        ( CollectionModel subModel, CollectionPresentation folderId ) ->
            UI.Article.Collection.view
                { cache = context.cache
                , folderId = folderId
                }
                subModel
                |> Html.map CollectionMsg

        ( ListingModel subModel, ListingPresentation selection window ) ->
            UI.Article.Listing.view
                { cache = context.cache
                , selection = selection
                , window = window
                }
                subModel
                |> Html.map ListingMsg

        ( DetailsModel subModel, DocumentPresentation maybeFolderId documentIdWithSearch ) ->
            UI.Article.Details.view
                { cache = context.cache
                , route = context.route
                , documentIdWithSearch = documentIdWithSearch
                }
                subModel
                |> Html.map DetailsMsg

        _ ->
            -- Model doesn't match query-context; should never happen.
            Html.text ""
