module Types.Presentation exposing
    ( Presentation(..)
    , getFolderId
    , fromRoute
    )

{-| In addition to the route, further knowledge about the types of the given nodes
may be necessary to choose which kind of article should be display in the UI.

Example:

  - The route `/1234` may refer to a collection, a folder (displayed as a listing of its documents)
    or a specific document.
  - The route `/1234/5678` will probably refer to a document within a folder.
    Then again one of the given ids may not exist in the database,
    or may reference an entity that is not of the respective type.
    In this case we want to display a special article with an appropriate error message.
  - For any route the app may not yet have received data about the resource to display.
    In this case we want to display a generic article with an appropriate message.

A [`Presentation`](#Presentation) describes the kind as well as the specific parameters of the article to be displayed
for the given route under the current knowledge about the relevant node types.

Each variant of the type corresponds to a sub-component of [`UI.Article`](UI-Article).

@docs Presentation
@docs getFolderId
@docs fromRoute

-}

import Cache exposing (Cache)
import Cache.Derive
import Maybe.Extra
import RemoteData
import Types exposing (DocumentIdWithSearch, FolderDisplay(..), NodeType(..), Window)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Route as Route exposing (Route)
import Types.Route.Filter
import Types.Selection exposing (SelectMethod(..), Selection)


{-| -}
type Presentation
    = GenericPresentation (Maybe ( NodeId, Maybe NodeId ))
    | CollectionPresentation FolderId
    | DocumentPresentation (Maybe FolderId) DocumentIdWithSearch
    | ListingPresentation Selection Window


{-| -}
getFolderId : Cache -> Presentation -> Maybe FolderId
getFolderId cache presentation =
    case presentation of
        GenericPresentation maybeNodeIds ->
            maybeNodeIds
                |> Maybe.andThen
                    (Tuple.first >> Cache.Derive.getAsFolderId cache)

        DocumentPresentation maybeFolderId documentId ->
            maybeFolderId

        CollectionPresentation folderId ->
            Just folderId

        ListingPresentation selection window ->
            Just selection.scope


{-| -}
fromRoute : Cache -> Route -> Presentation
fromRoute cache route =
    let
        folderPresentation folderId folderType =
            case selectMethodOfRoute of
                SelectByFolderListing ->
                    case folderType of
                        DisplayAsCollection ->
                            CollectionPresentation folderId

                        DisplayAsDirectory ->
                            ListingPresentation
                                { scope = folderId
                                , selectMethod = SelectByFolderListing
                                , filters = Types.Route.Filter.fromRoute route
                                , facetFilters = route.parameters.facetFilters
                                }
                                windowOfRoute

                selectMethod ->
                    ListingPresentation
                        { scope = folderId
                        , selectMethod = selectMethod
                        , filters = Types.Route.Filter.fromRoute route
                        , facetFilters = route.parameters.facetFilters
                        }
                        windowOfRoute

        windowOfRoute =
            { offset = route.parameters.offset
            , limit = route.parameters.limit
            }

        selectMethodOfRoute =
            case route.parameters.ftsTerm of
                Nothing ->
                    SelectByFolderListing

                Just ftsTerm ->
                    SelectByFullTextSearch
                        ftsTerm
                        route.parameters.ftsSorting
    in
    case route.path of
        Route.NoId ->
            Cache.Derive.getRootFolder cache
                |> RemoteData.toMaybe
                |> Maybe.Extra.unwrap
                    (GenericPresentation Nothing)
                    (\( rootFolderId, rootFolderType ) ->
                        folderPresentation rootFolderId rootFolderType
                    )

        Route.OneId nodeId ->
            case
                Cache.Derive.getNodeType cache nodeId
                    |> RemoteData.toMaybe
            of
                Nothing ->
                    GenericPresentation (Just ( nodeId, Nothing ))

                Just NodeIsNeither ->
                    GenericPresentation (Just ( nodeId, Nothing ))

                Just NodeIsDocument ->
                    DocumentPresentation Nothing
                        (DocumentIdWithSearch
                            (nodeId |> Id.asDocumentId)
                            Nothing
                        )

                Just (NodeIsFolder folderType) ->
                    folderPresentation
                        (nodeId |> Id.asFolderId)
                        folderType

        Route.TwoIds nodeIdOne nodeIdTwo ->
            case
                RemoteData.map2 Tuple.pair
                    (Cache.Derive.getNodeType cache nodeIdOne)
                    (Cache.Derive.getNodeType cache nodeIdTwo)
                    |> RemoteData.toMaybe
            of
                Just ( NodeIsFolder folderType, NodeIsDocument ) ->
                    DocumentPresentation
                        (Just (nodeIdOne |> Id.asFolderId))
                        (DocumentIdWithSearch
                            (nodeIdTwo |> Id.asDocumentId)
                            Nothing
                        )

                _ ->
                    GenericPresentation (Just ( nodeIdOne, Just nodeIdTwo ))
