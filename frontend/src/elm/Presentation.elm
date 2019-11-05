module Presentation exposing
    ( Presentation(..)
    , fromRoute
    , getFolderId
    , toRoute
    , view
    )

import Data.Cache as Cache
import Data.Types exposing (..)
import Dict
import Document
import Folder
import Html exposing (Html)
import List.Extra
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Query.Attribute
import Query.Filter as Filter
import Query.Filters as Filters
import RemoteData
import Route exposing (Route)
import Set
import String.Extra
import Utils


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
                    (Tuple.first >> Cache.getAsFolderId cache)

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
                        FolderIsCollection ->
                            CollectionPresentation folderId

                        FolderIsDirectory ->
                            DocumentsPagePresentation
                                { scope = folderId
                                , searchMethod = SelectByFolderListing
                                , filters = Filters.filtersFromRoute route
                                }
                                (windowFromRoute route)

                searchMethod ->
                    DocumentsPagePresentation
                        { scope = folderId
                        , searchMethod = searchMethod
                        , filters = Filters.filtersFromRoute route
                        }
                        (windowFromRoute route)
    in
    case route.path of
        Route.NoId ->
            Cache.getRootFolder cache
                |> RemoteData.toMaybe
                |> Maybe.Extra.unwrap
                    (GenericPresentation Nothing)
                    (\( rootFolderId, rootFolderType ) ->
                        folderPresentation rootFolderId rootFolderType
                    )

        Route.OneId nodeId ->
            case
                Cache.getNodeType cache nodeId
                    |> RemoteData.toMaybe
            of
                Nothing ->
                    GenericPresentation (Just ( nodeId, Nothing ))

                Just NodeIsNeither ->
                    GenericPresentation (Just ( nodeId, Nothing ))

                Just NodeIsDocument ->
                    DocumentPresentation Nothing (nodeId |> nodeIdToInt |> documentIdFromInt)

                Just (NodeIsFolder folderType) ->
                    folderPresentation
                        (nodeId |> nodeIdToInt |> folderIdFromInt)
                        folderType

        Route.TwoIds nodeIdOne nodeIdTwo ->
            case
                RemoteData.map2 Tuple.pair
                    (Cache.getNodeType cache nodeIdOne)
                    (Cache.getNodeType cache nodeIdTwo)
                    |> RemoteData.toMaybe
            of
                Just ( NodeIsFolder folderType, NodeIsDocument ) ->
                    DocumentPresentation
                        (Just (nodeIdOne |> nodeIdToInt |> folderIdFromInt))
                        (nodeIdTwo |> nodeIdToInt |> documentIdFromInt)

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


toRoute : Presentation -> Route
toRoute presentation =
    -- TODO: implement
    -- TODO: really needed? Possibly only for testing?
    Route.home


view : Presentation -> Html Never
view presentation =
    -- TODO: needed? Possibly handy in some form for UX.
    Html.text ""
