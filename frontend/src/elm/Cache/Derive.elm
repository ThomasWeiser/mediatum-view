module Cache.Derive exposing
    ( DerivedData
    , Error(..)
    , errorToString
    , asDerivedData
    , getNodeType
    , getAsFolderId
    , getAsDocumentId
    , getFolderDisplay
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
@docs getFolderDisplay
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
getFolderDisplay : Cache -> FolderId -> Maybe FolderDisplay
getFolderDisplay cache folderId =
    case Cache.get cache.nodeTypes (folderId |> Id.asNodeId) of
        Success (NodeIsFolder folderDisplay) ->
            Just folderDisplay

        _ ->
            Nothing


{-| -}
getParentId : Config -> Cache -> FolderId -> ApiData (Maybe FolderId)
getParentId config cache id =
    if List.member id config.toplevelFolderIds then
        Success Nothing

    else
        Cache.get cache.folders id
            |> RemoteData.map .parent


{-| -}
getPath : Config -> Cache -> FolderId -> ApiData (List FolderId)
getPath config cache id =
    getParentId config cache id
        |> RemoteData.andThen
            (Maybe.Extra.unwrap
                (RemoteData.Success [ id ])
                (getPath config cache
                    >> RemoteData.map ((::) id)
                )
            )


{-| -}
getPathAsFarAsCached : Config -> Cache -> Maybe FolderId -> List FolderId
getPathAsFarAsCached config cache =
    Maybe.Extra.unwrap
        []
        (\id ->
            id
                :: (getParentId config cache id
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> getPathAsFarAsCached config cache
                   )
        )


{-| -}
isOnPath : Config -> Cache -> FolderId -> Maybe FolderId -> Bool
isOnPath config cache requestedId =
    Maybe.Extra.unwrap
        False
        (\pathId ->
            (requestedId == pathId)
                || isOnPath config
                    cache
                    requestedId
                    (getParentId config cache pathId
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
folderCountsOnPath : Config -> Cache -> Selection -> FolderCounts
folderCountsOnPath config cache selection =
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
        (getPathAsFarAsCached config cache (Just selection.scope))
