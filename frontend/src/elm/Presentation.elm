module Presentation exposing (Presentation(..), fromRoute, showFilters, toRoute, view)

import Data.Cache as Cache
import Data.Types exposing (..)
import Data.Utils
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


showFilters : Presentation -> Bool
showFilters presentation =
    case presentation of
        DocumentsPagePresentation _ _ ->
            True

        _ ->
            False


getNodeType : Cache.Model -> NodeId -> Maybe NodeType
getNodeType cache nodeId =
    Cache.get cache.nodeTypes nodeId
        |> RemoteData.toMaybe


getRootFolder : Cache.Model -> Maybe ( FolderId, FolderType )
getRootFolder cache =
    cache.rootFolderIds
        |> RemoteData.toMaybe
        |> Maybe.andThen List.head
        |> Maybe.andThen
            (\folderId ->
                getNodeType cache (folderId |> folderIdToInt |> nodeIdFromInt)
                    |> Maybe.andThen
                        (\nodeType ->
                            case nodeType of
                                NodeIsFolder folderType ->
                                    Just ( folderId, folderType )

                                _ ->
                                    Nothing
                        )
            )


fromRoute : Cache.Model -> Route -> Presentation
fromRoute cache route =
    case route.path of
        Route.NoId ->
            getRootFolder cache
                |> Maybe.Extra.unwrap
                    (GenericPresentation Nothing)
                    (\( rootFolderId, rootFolderType ) ->
                        case rootFolderType of
                            FolderIsCollection ->
                                CollectionPresentation rootFolderId

                            FolderIsDirectory ->
                                DocumentsPagePresentation
                                    { scope = rootFolderId
                                    , searchMethod = searchMethodFromRoute route
                                    , filters = filtersFromRoute route
                                    }
                                    (windowFromRoute route)
                    )

        Route.OneId nodeId ->
            case getNodeType cache nodeId of
                Nothing ->
                    GenericPresentation (Just ( nodeId, Nothing ))

                Just NodeIsNeither ->
                    GenericPresentation (Just ( nodeId, Nothing ))

                Just NodeIsDocument ->
                    DocumentPresentation Nothing (nodeId |> nodeIdToInt |> documentIdFromInt)

                Just (NodeIsFolder FolderIsCollection) ->
                    CollectionPresentation (nodeId |> nodeIdToInt |> folderIdFromInt)

                Just (NodeIsFolder FolderIsDirectory) ->
                    DocumentsPagePresentation
                        { scope = nodeId |> nodeIdToInt |> folderIdFromInt
                        , searchMethod = searchMethodFromRoute route
                        , filters = filtersFromRoute route
                        }
                        (windowFromRoute route)

        Route.TwoIds nodeIdOne nodeIdTwo ->
            case ( getNodeType cache nodeIdOne, getNodeType cache nodeIdTwo ) of
                ( Just (NodeIsFolder folderType), Just NodeIsDocument ) ->
                    DocumentPresentation
                        (Just (nodeIdOne |> nodeIdToInt |> folderIdFromInt))
                        (nodeIdTwo |> nodeIdToInt |> documentIdFromInt)

                _ ->
                    GenericPresentation (Just ( nodeIdOne, Just nodeIdTwo ))


windowFromRoute : Route -> Window
windowFromRoute route =
    { offset = route.parameters.offset
    , limit = route.parameters.offset
    }


searchMethodFromRoute : Route -> SearchMethod
searchMethodFromRoute route =
    case String.Extra.nonBlank route.parameters.ftsTerm of
        Nothing ->
            SelectByFolderListing

        Just ftsTerm ->
            SelectByFullTextSearch
                ftsTerm
                route.parameters.ftsSorting


filtersFromRoute : Route -> Filters
filtersFromRoute route =
    Set.toList route.parameters.filterByTitle
        |> List.map FilterTitleFts
        |> Utils.prependMaybe
            (route.parameters.filterByYear
                |> Maybe.map FilterYearWithin
            )
        |> List.map (\filter -> ( Filter.handle filter, filter ))
        |> Dict.fromList


toRoute : Presentation -> Route
toRoute presentation =
    -- TODO: implement
    -- TODO: really needed? Possibly only for testing?
    Route.home


view : Presentation -> Html Never
view presentation =
    -- TODO: implement
    -- TODO: needed? Possibly handy in some form for UX.
    Html.div [] [ Html.text <| Debug.toString presentation ]



{-
   Html.div [] <|
       case query of
           OnDetails _ ->
               []

           OnFolder { folder } ->
               case folder.type_ of
                   Data.Types.FolderIsCollection ->
                       []

                   Data.Types.FolderIsDirectory ->
                       [ Html.div []
                           [ Html.span [] [ Html.text "All Documents" ] ]
                       ]

           OnFts { searchTerm } ->
               [ Html.div []
                   [ Html.span [] [ Html.text "Search: \"" ]
                   , Html.span [] [ Html.text searchTerm ]
                   , Html.span [] [ Html.text "\"" ]
                   ]
               ]
-}
