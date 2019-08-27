module Article exposing
    ( Model
    , Msg
    , Return(..)
    , folderCountsForQuery
    , initialModel
    , needs
    , update
    , view
    )

import Article.Collection
import Article.Details
import Article.DocumentsPage
import Article.Generic
import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (..)
import Data.Utils
import Html exposing (Html)
import Html.Attributes
import Navigation exposing (Navigation)
import Presentation exposing (Presentation(..))
import Query exposing (Query)
import Query.Filter
import Query.Filters
import RemoteData
import Tree
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
    = GenericModel Article.Generic.Model
    | CollectionModel Article.Collection.Model
    | DocumentsPageModel Article.DocumentsPage.Model
    | DetailsModel Article.Details.Model


type Msg
    = GenericMsg Article.Generic.Msg
    | CollectionMsg Article.Collection.Msg
    | DocumentsPageMsg Article.DocumentsPage.Msg
    | DetailsMsg Article.Details.Msg


initialModel : Context -> Model
initialModel context =
    case context.presentation of
        GenericPresentation maybeNodeIds ->
            { content = GenericModel Article.Generic.initialModel }

        DocumentPresentation maybeFolderId documentId ->
            { content = DetailsModel Article.Details.initialModel }

        CollectionPresentation folderId ->
            { content = CollectionModel Article.Collection.initialModel }

        DocumentsPagePresentation selection window ->
            { content = DocumentsPageModel Article.DocumentsPage.initialModel }


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

        DocumentsPagePresentation selection window ->
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

        DocumentsPagePresentation selection window ->
            Cache.get context.cache.folderCounts selection
                |> RemoteData.withDefault Data.Utils.folderCountsInit
                |> Just


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case ( msg, model.content, context.presentation ) of
        ( GenericMsg subMsg, GenericModel subModel, _ ) ->
            let
                ( subModel1, subCmd ) =
                    Article.Generic.update subMsg subModel
            in
            ( { model | content = GenericModel subModel1 }
            , Cmd.map GenericMsg subCmd
            , NoReturn
            )

        ( CollectionMsg subMsg, CollectionModel subModel, _ ) ->
            let
                ( subModel1, subCmd ) =
                    Article.Collection.update subMsg subModel
            in
            ( { model | content = CollectionModel subModel1 }
            , Cmd.map CollectionMsg subCmd
            , NoReturn
            )

        ( DocumentsPageMsg subMsg, DocumentsPageModel subModel, DocumentsPagePresentation selection window ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    Article.DocumentsPage.update
                        { cache = context.cache
                        , selection = selection
                        , window = window
                        }
                        subMsg
                        subModel
            in
            ( { model | content = DocumentsPageModel subModel1 }
            , Cmd.map DocumentsPageMsg subCmd
            )
                |> Utils.tupleAddThird
                    (case subReturn of
                        Article.DocumentsPage.NoReturn ->
                            NoReturn

                        Article.DocumentsPage.Navigate navigation ->
                            Navigate navigation
                    )

        ( DetailsMsg subMsg, DetailsModel subModel, DocumentPresentation maybeFolderId documentId ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    Article.Details.update
                        { cache = context.cache
                        , documentId = documentId
                        }
                        subMsg
                        subModel
            in
            ( { model | content = DetailsModel subModel1 }
            , Cmd.map DetailsMsg subCmd
            , case subReturn of
                Article.Details.NoReturn ->
                    NoReturn

                Article.Details.UpdateCacheWithModifiedDocument document ->
                    UpdateCacheWithModifiedDocument document
            )

        _ ->
            -- Message doesn't match model; shouldn't never happen
            -- Or model doesn't match presentation; TODO: Can this happen?
            ( model, Cmd.none, NoReturn )


view : Tree.Model -> Context -> Model -> Html Msg
view tree context model =
    Html.article
        [ Html.Attributes.class "article" ]
        [ Html.div
            [ Html.Attributes.class "breadcrumbs" ]
            [ Tree.viewBreadcrumbs
                { cache = context.cache
                , presentation = context.presentation
                }
                tree
                (Presentation.getFolderId context.cache context.presentation)
            ]
        , Presentation.view context.presentation
            |> Html.map never
        , Html.div []
            [ Html.text "Article.content: "
            , Html.text <| Debug.toString model.content
            ]
        , viewContent context model
        ]


viewContent : Context -> Model -> Html Msg
viewContent context model =
    case ( model.content, context.presentation ) of
        ( GenericModel subModel, GenericPresentation nodeIds ) ->
            Article.Generic.view
                { cache = context.cache
                , nodeIds = nodeIds
                }
                subModel
                |> Html.map GenericMsg

        ( CollectionModel subModel, CollectionPresentation folderId ) ->
            Article.Collection.view
                { cache = context.cache
                , folderId = folderId
                }
                subModel
                |> Html.map CollectionMsg

        ( DocumentsPageModel subModel, DocumentsPagePresentation selection window ) ->
            Article.DocumentsPage.view
                { cache = context.cache
                , selection = selection
                , window = window
                }
                subModel
                |> Html.map DocumentsPageMsg

        ( DetailsModel subModel, DocumentPresentation maybeFolderId documentId ) ->
            Article.Details.view
                { cache = context.cache
                , documentId = documentId
                }
                subModel
                |> Html.map DetailsMsg

        _ ->
            -- Model doesn't match query-context; TODO: Can this happen?
            Html.text ""
