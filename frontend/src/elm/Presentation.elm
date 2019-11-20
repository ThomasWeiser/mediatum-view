module Presentation exposing
    ( Presentation(..)
    , fromRoute
    , getFolderId
    )

import Cache
import Cache.Derive
import Entities.Folder as Folder exposing (Folder)
import Maybe.Extra
import Query.Filters as Filters
import RemoteData
import Route exposing (Route)
import Types exposing (FolderDisplay(..), NodeType(..), Window)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Selection exposing (SearchMethod(..), Selection)


type Presentation
    = GenericPresentation (Maybe ( NodeId, Maybe NodeId ))
    | CollectionPresentation FolderId
    | DocumentPresentation (Maybe FolderId) DocumentId
    | DocumentsPagePresentation Selection Window


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

        DocumentsPagePresentation selection window ->
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
                            DocumentsPagePresentation
                                { scope = folderId
                                , searchMethod = SelectByFolderListing
                                , filters = Filters.fromRoute route
                                }
                                (windowFromRoute route)

                searchMethod ->
                    DocumentsPagePresentation
                        { scope = folderId
                        , searchMethod = searchMethod
                        , filters = Filters.fromRoute route
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
                    DocumentPresentation Nothing (nodeId |> Id.toInt |> Id.fromInt)

                Just (NodeIsFolder folderType) ->
                    folderPresentation
                        (nodeId |> Id.toInt |> Id.fromInt)
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
                        (Just (nodeIdOne |> Id.toInt |> Id.fromInt))
                        (nodeIdTwo |> Id.toInt |> Id.fromInt)

                _ ->
                    GenericPresentation (Just ( nodeIdOne, Just nodeIdTwo ))


windowFromRoute : Route -> Window
windowFromRoute route =
    { offset = route.parameters.offset
    , limit = route.parameters.limit
    }


searchMethodFromRoute : Route -> SearchMethod
searchMethodFromRoute route =
    case route.parameters.ftsTerm of
        Nothing ->
            SelectByFolderListing

        Just ftsTerm ->
            SelectByFullTextSearch
                ftsTerm
                route.parameters.ftsSorting
