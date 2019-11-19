module Presentation exposing
    ( Presentation(..)
    , fromRoute
    , getFolderId
    )

import Data.Cache as Cache
import Data.Derive
import Maybe.Extra
import Query.Filters as Filters
import RemoteData
import Route exposing (Route)
import Types.Folder as Folder exposing (Folder)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.NodeType exposing (NodeType(..))
import Types.Selection exposing (SearchMethod(..), Selection)
import Types.Window exposing (Window)


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
                    (Tuple.first >> Data.Derive.getAsFolderId cache)

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
                        Folder.IsCollection ->
                            CollectionPresentation folderId

                        Folder.IsDirectory ->
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
            Data.Derive.getRootFolder cache
                |> RemoteData.toMaybe
                |> Maybe.Extra.unwrap
                    (GenericPresentation Nothing)
                    (\( rootFolderId, rootFolderType ) ->
                        folderPresentation rootFolderId rootFolderType
                    )

        Route.OneId nodeId ->
            case
                Data.Derive.getNodeType cache nodeId
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
                    (Data.Derive.getNodeType cache nodeIdOne)
                    (Data.Derive.getNodeType cache nodeIdTwo)
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
