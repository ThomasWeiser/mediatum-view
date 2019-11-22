module Presentation exposing
    ( Presentation(..)
    , fromRoute
    , getFolderId
    )

import Cache
import Cache.Derive
import Maybe.Extra
import RemoteData
import Route exposing (Route)
import Route.Filter
import Types exposing (FolderDisplay(..), NodeType(..), Window)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Selection exposing (SelectMethod(..), Selection)


type Presentation
    = GenericPresentation (Maybe ( NodeId, Maybe NodeId ))
    | CollectionPresentation FolderId
    | DocumentPresentation (Maybe FolderId) DocumentId
    | ListingPresentation Selection Window


getFolderId : Cache.Model -> Presentation -> Maybe FolderId
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


fromRoute : Cache.Model -> Route -> Presentation
fromRoute cache route =
    let
        folderPresentation folderId folderType =
            case searchMethodFromRoute route of
                SelectByFolderListing ->
                    case folderType of
                        DisplayAsCollection ->
                            CollectionPresentation folderId

                        DisplayAsDirectory ->
                            ListingPresentation
                                { scope = folderId
                                , selectMethod = SelectByFolderListing
                                , filters = Route.Filter.fromRoute route
                                }
                                (windowFromRoute route)

                selectMethod ->
                    ListingPresentation
                        { scope = folderId
                        , selectMethod = selectMethod
                        , filters = Route.Filter.fromRoute route
                        }
                        (windowFromRoute route)
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
                    DocumentPresentation Nothing (nodeId |> Id.asDocumentId)

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
                        (nodeIdTwo |> Id.asDocumentId)

                _ ->
                    GenericPresentation (Just ( nodeIdOne, Just nodeIdTwo ))


windowFromRoute : Route -> Window
windowFromRoute route =
    { offset = route.parameters.offset
    , limit = route.parameters.limit
    }


searchMethodFromRoute : Route -> SelectMethod
searchMethodFromRoute route =
    case route.parameters.ftsTerm of
        Nothing ->
            SelectByFolderListing

        Just ftsTerm ->
            SelectByFullTextSearch
                ftsTerm
                route.parameters.ftsSorting
