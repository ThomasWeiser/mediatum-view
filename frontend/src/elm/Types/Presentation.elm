module Types.Presentation exposing
    ( Presentation(..)
    , getFolderId, getSelection
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
@docs getFolderId, getSelection
@docs fromRoute

-}

import Cache exposing (Cache)
import Cache.Derive
import Maybe.Extra
import RemoteData
import Types exposing (DocumentIdFromSearch, FolderDisplay(..), NodeType(..))
import Types.Config exposing (Config)
import Types.FilterList as FilterList
import Types.Id as Id exposing (FolderId, NodeId)
import Types.Route as Route exposing (Route)
import Types.Selection exposing (Selection)


{-| -}
type Presentation
    = GenericPresentation (Maybe ( NodeId, Maybe DocumentIdFromSearch ))
    | CollectionPresentation FolderId
    | DocumentPresentation (Maybe FolderId) DocumentIdFromSearch
    | ListingPresentation Selection Int
    | IteratorPresentation Selection Int DocumentIdFromSearch


{-| -}
getFolderId : Cache -> Presentation -> Maybe FolderId
getFolderId cache presentation =
    case presentation of
        GenericPresentation maybeNodeIds ->
            maybeNodeIds
                |> Maybe.andThen
                    (Tuple.first >> Cache.Derive.getAsFolderId cache)

        DocumentPresentation maybeFolderId documentIdFromSearch ->
            maybeFolderId

        CollectionPresentation folderId ->
            Just folderId

        ListingPresentation selection limit ->
            Just selection.scope

        IteratorPresentation selection limit documentIdFromSearch ->
            Just selection.scope


{-| -}
getSelection : Presentation -> Maybe Selection
getSelection presentation =
    case presentation of
        GenericPresentation _ ->
            Nothing

        DocumentPresentation _ _ ->
            Nothing

        CollectionPresentation _ ->
            Nothing

        ListingPresentation selection _ ->
            Just selection

        IteratorPresentation selection _ _ ->
            Just selection


{-| -}
fromRoute : Config -> Cache -> Route -> Presentation
fromRoute config cache route =
    let
        cannotShowListing folderType =
            (route.parameters.globalFts == Nothing)
                && FilterList.isEmpty route.parameters.ftsFilters
                && (folderType == DisplayAsCollection)

        folderPresentation folderId folderType =
            if cannotShowListing folderType then
                CollectionPresentation folderId

            else
                ListingPresentation
                    { scope = folderId
                    , globalFts = route.parameters.globalFts
                    , ftsFilters = route.parameters.ftsFilters
                    , facetFilters = route.parameters.facetFilters
                    , sorting = route.parameters.sorting
                    }
                    route.parameters.limit
    in
    case route.path of
        Route.NoId ->
            List.head config.toplevelFolderIds
                |> Maybe.Extra.unwrap
                    (GenericPresentation Nothing)
                    (\firstToplevelFolderId ->
                        Cache.Derive.getFolderDisplay cache firstToplevelFolderId
                            |> Maybe.Extra.unwrap
                                (GenericPresentation (Just ( firstToplevelFolderId |> Id.asNodeId, Nothing )))
                                (folderPresentation firstToplevelFolderId)
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
                        (DocumentIdFromSearch
                            (nodeId |> Id.asDocumentId)
                            route.parameters.globalFts
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
                    if cannotShowListing folderType then
                        DocumentPresentation
                            (Just (nodeIdOne |> Id.asFolderId))
                            (DocumentIdFromSearch
                                (nodeIdTwo |> Id.asDocumentId)
                                route.parameters.globalFts
                            )

                    else
                        IteratorPresentation
                            { scope = nodeIdOne |> Id.asFolderId
                            , globalFts = route.parameters.globalFts
                            , ftsFilters = route.parameters.ftsFilters
                            , facetFilters = route.parameters.facetFilters
                            , sorting = route.parameters.sorting
                            }
                            route.parameters.limit
                            (DocumentIdFromSearch
                                (nodeIdTwo |> Id.asDocumentId)
                                route.parameters.globalFts
                            )

                _ ->
                    GenericPresentation
                        (Just
                            ( nodeIdOne
                            , Just
                                (DocumentIdFromSearch
                                    (nodeIdTwo |> Id.asDocumentId)
                                    route.parameters.globalFts
                                )
                            )
                        )
