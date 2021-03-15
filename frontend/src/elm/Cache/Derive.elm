module Cache.Derive exposing
    ( DerivedData
    , Error(..)
    , errorToString
    , asDerivedData
    , getNodeType
    , getAsFolderId
    , getAsDocumentId
    , getRootFolder
    , getRootFolderId
    , getParentId
    , getPath
    , getPathAsFarAsCached
    , isOnPath
    , getDocumentCount
    , folderCountsOnPath
    )

{-| Functions for getting/deriving some special data from the base tables in the cache.


# General types / Error handling

@docs DerivedData
@docs Error

@docs errorToString
@docs asDerivedData


# Derivation functions

@docs getNodeType
@docs getAsFolderId
@docs getAsDocumentId
@docs getRootFolder
@docs getRootFolderId
@docs getParentId
@docs getPath
@docs getPathAsFarAsCached
@docs isOnPath
@docs getDocumentCount
@docs folderCountsOnPath

-}

import Cache exposing (ApiData, Cache)
import Entities.FolderCounts as FolderCounts exposing (FolderCounts)
import Maybe.Extra
import RemoteData exposing (RemoteData(..))
import Sort.Dict
import Types exposing (FolderDisplay(..), NodeType(..))
import Types.Config exposing (Config)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Selection exposing (Selection)


{-| A specialization of [`RemoteData e a`](/packages/krisajenkins/remotedata/6.0.1/RemoteData#RemoteData)
where the error type `e` is defined by [`Cache.Derive.Error`](#Error).

Functions of this module that may result in a `CacheDerivationError` return data wrapped in this type.

-}
type alias DerivedData a =
    RemoteData Error a


{-| An error on getting derived data may be either an ApiError or some logical error within the base data.
-}
type Error
    = CacheApiError Cache.ApiError
    | CacheDerivationError String


{-| -}
errorToString : Error -> String
errorToString error =
    case error of
        CacheApiError apiError ->
            Cache.apiErrorToString apiError

        CacheDerivationError str ->
            str


{-| Lift an ApiData into a DerivedData. Used to extend the error type.
-}
asDerivedData : ApiData a -> DerivedData a
asDerivedData =
    RemoteData.mapError CacheApiError


{-| -}
getNodeType : Cache -> NodeId -> ApiData NodeType
getNodeType cache nodeId =
    Cache.get cache.nodeTypes nodeId


{-| -}
getAsFolderId : Cache -> NodeId -> Maybe FolderId
getAsFolderId cache nodeId =
    case Cache.get cache.nodeTypes nodeId of
        Success (NodeIsFolder _) ->
            nodeId |> Id.asFolderId |> Just

        _ ->
            Nothing


{-| -}
getAsDocumentId : Cache -> NodeId -> Maybe DocumentId
getAsDocumentId cache nodeId =
    case Cache.get cache.nodeTypes nodeId of
        Success NodeIsDocument ->
            nodeId |> Id.asDocumentId |> Just

        _ ->
            Nothing


{-| -}
getRootFolder : Config -> Cache -> DerivedData ( FolderId, FolderDisplay )
getRootFolder config cache =
    config.toplevelFolderIds
        |> List.head
        |> RemoteData.fromMaybe (CacheDerivationError "List of root folders is empty")
        |> RemoteData.andThen
            (\folderId ->
                Cache.get cache.nodeTypes (folderId |> Id.asNodeId)
                    |> RemoteData.mapError CacheApiError
                    |> RemoteData.andThen
                        (\nodeType ->
                            case nodeType of
                                NodeIsFolder folderType ->
                                    Success ( folderId, folderType )

                                _ ->
                                    Failure (CacheDerivationError "Root node is not a folder")
                        )
            )


{-| -}
getRootFolderId : Config -> Cache -> Maybe FolderId
getRootFolderId config cache =
    getRootFolder config cache
        |> RemoteData.toMaybe
        |> Maybe.map Tuple.first


{-| -}
getParentId : Cache -> FolderId -> ApiData (Maybe FolderId)
getParentId cache id =
    Cache.get cache.folders id
        |> RemoteData.map .parent


{-| -}
getPath : Cache -> FolderId -> ApiData (List FolderId)
getPath cache id =
    getParentId cache id
        |> RemoteData.andThen
            (Maybe.Extra.unwrap
                (RemoteData.Success [ id ])
                (getPath cache
                    >> RemoteData.map ((::) id)
                )
            )


{-| -}
getPathAsFarAsCached : Cache -> Maybe FolderId -> List FolderId
getPathAsFarAsCached cache =
    Maybe.Extra.unwrap
        []
        (\id ->
            id
                :: (getParentId cache id
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> getPathAsFarAsCached cache
                   )
        )


{-| -}
isOnPath : Cache -> FolderId -> Maybe FolderId -> Bool
isOnPath cache requestedId =
    Maybe.Extra.unwrap
        False
        (\pathId ->
            (requestedId == pathId)
                || isOnPath cache
                    requestedId
                    (getParentId cache pathId
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                    )
        )


{-| -}
getDocumentCount : Cache -> Selection -> DerivedData Int
getDocumentCount cache selection =
    Cache.get cache.folderCounts selection
        |> RemoteData.mapError CacheApiError
        |> RemoteData.andThen
            (\folderCounts ->
                case Sort.Dict.get selection.scope folderCounts of
                    Just count ->
                        Success count

                    Nothing ->
                        Failure (CacheDerivationError "Missing count for scoped folder")
            )


{-| Get the folder counts for the scoped folder of the selection and of all folders upwards to the root.
-}
folderCountsOnPath : Cache -> Selection -> FolderCounts
folderCountsOnPath cache selection =
    List.foldr
        (\folderId folderCounts ->
            Sort.Dict.foldl Sort.Dict.insert
                (Cache.get
                    cache.folderCounts
                    { selection | scope = folderId }
                    |> RemoteData.withDefault FolderCounts.init
                )
                folderCounts
        )
        FolderCounts.init
        (getPathAsFarAsCached cache (Just selection.scope))
