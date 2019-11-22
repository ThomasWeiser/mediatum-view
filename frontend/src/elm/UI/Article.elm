module UI.Article exposing
    ( Model
    , Msg
    , Return(..)
    , folderCountsForQuery
    , initialModel
    , needs
    , update
    , view
    )

import Cache
import Cache.Derive
import Entities.Document exposing (Document)
import Entities.FolderCounts as FolderCounts exposing (FolderCounts)
import Html exposing (Html)
import Html.Attributes
import Navigation exposing (Navigation)
import Presentation exposing (Presentation(..))
import RemoteData
import Route
import Route.Url
import Types.Id as Id exposing (FolderId, NodeId)
import UI.Article.Collection
import UI.Article.Details
import UI.Article.Generic
import UI.Article.Listing
import UI.Tree
import Utils


type alias Context =
    { cache : Cache.Model
    , presentation : Presentation
    }


type Return
    = NoReturn
    | Navigate Navigation
    | UpdateCacheWithModifiedDocument Document


type alias Model =
    { content : Content
    }


type Content
    = GenericModel UI.Article.Generic.Model
    | CollectionModel UI.Article.Collection.Model
    | ListingModel UI.Article.Listing.Model
    | DetailsModel UI.Article.Details.Model


type Msg
    = GenericMsg UI.Article.Generic.Msg
    | CollectionMsg UI.Article.Collection.Msg
    | ListingMsg UI.Article.Listing.Msg
    | DetailsMsg UI.Article.Details.Msg


initialModel : Context -> Model
initialModel context =
    case context.presentation of
        GenericPresentation maybeNodeIds ->
            { content = GenericModel UI.Article.Generic.initialModel }

        DocumentPresentation maybeFolderId documentId ->
            { content = DetailsModel UI.Article.Details.initialModel }

        CollectionPresentation folderId ->
            { content = CollectionModel UI.Article.Collection.initialModel }

        ListingPresentation selection window ->
            { content = ListingModel UI.Article.Listing.initialModel }


needs : Context -> Cache.Needs
needs context =
    case context.presentation of
        GenericPresentation maybeNodeIds ->
            -- TODO: Do we want to declare the NeedGenericNode here?
            --       Currently we already declare these needs in App.needs.
            case maybeNodeIds of
                Nothing ->
                    Cache.NeedNothing

                Just ( nodeIdOne, maybeNodeIdTwo ) ->
                    Cache.NeedAnd
                        (Cache.NeedGenericNode nodeIdOne)
                        (case maybeNodeIdTwo of
                            Nothing ->
                                Cache.NeedNothing

                            Just nodeIdTwo ->
                                Cache.NeedGenericNode nodeIdTwo
                        )

        DocumentPresentation maybeFolderId documentId ->
            Cache.NeedDocument documentId

        CollectionPresentation folderId ->
            -- TODO: Should there be a need for a folder?
            Cache.NeedNothing

        ListingPresentation selection window ->
            -- TODO: We currently don't observe the needs of an Iterator
            Cache.NeedAndThen
                (Cache.NeedDocumentsPage selection window)
                (Cache.NeedFolderCounts selection)


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

        ( DetailsMsg subMsg, DetailsModel subModel, DocumentPresentation maybeFolderId documentId ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    UI.Article.Details.update
                        { cache = context.cache
                        , documentId = documentId
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
            -- Message doesn't match model; shouldn't never happen
            -- Or model doesn't match presentation; TODO: Can this happen?
            ( model, Cmd.none, NoReturn )


view : Context -> Model -> Html Msg
view context model =
    Html.article
        [ Html.Attributes.class "article" ]
        [ Html.div
            [ Html.Attributes.class "breadcrumbs" ]
            [ viewBreadcrumbs
                { cache = context.cache
                , presentation = context.presentation
                }
                (Presentation.getFolderId context.cache context.presentation)
            ]
        , viewContent context model
        ]


viewBreadcrumbs : Context -> Maybe FolderId -> Html msg
viewBreadcrumbs context maybeFolderId =
    Html.span [] <|
        case maybeFolderId of
            Nothing ->
                [ Html.text "(no specific path)" ]

            Just folderId ->
                Cache.Derive.getPath context.cache folderId
                    |> RemoteData.unwrap
                        [ Html.text "..." ]
                        (List.reverse
                            >> List.map
                                (\idPathSegment ->
                                    Html.span []
                                        [ Cache.get context.cache.folders idPathSegment
                                            |> RemoteData.unwrap
                                                (Html.text "...")
                                                (\folder ->
                                                    Html.a
                                                        [ folder.id
                                                            |> Id.asNodeId
                                                            |> Route.fromOneId
                                                            |> Route.Url.toString
                                                            |> Html.Attributes.href
                                                        ]
                                                        [ Html.text folder.name ]
                                                )
                                        ]
                                )
                            >> List.intersperse
                                (Html.span [] [ Html.text " > " ])
                        )


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

        ( DetailsModel subModel, DocumentPresentation maybeFolderId documentId ) ->
            UI.Article.Details.view
                { cache = context.cache
                , documentId = documentId
                }
                subModel
                |> Html.map DetailsMsg

        _ ->
            -- Model doesn't match query-context; TODO: Can this happen?
            Html.text ""
