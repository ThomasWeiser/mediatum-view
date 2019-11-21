module Cache.Derive exposing
    ( DerivedData
    , asDerivedData
    , getAsDocumentId
    , getAsFolderId
    , getNodeType
    , getParentId
    , getPath
    , getPathAsFarAsCached
    , getRootFolder
    , isOnPath
    )

import Cache exposing (ApiData, Error(..))
import Entities.Folder as Folder exposing (Folder)
import Maybe.Extra
import RemoteData exposing (RemoteData(..))
import Types exposing (FolderDisplay(..), NodeType(..))
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)


type alias DerivedData a =
    RemoteData Error a


asDerivedData : ApiData a -> DerivedData a
asDerivedData =
    RemoteData.mapError CacheApiError


getNodeType : Cache.Model -> NodeId -> ApiData NodeType
getNodeType cache nodeId =
    Cache.get cache.nodeTypes nodeId


getAsFolderId : Cache.Model -> NodeId -> Maybe FolderId
getAsFolderId cache nodeId =
    case Cache.get cache.nodeTypes nodeId of
        Success (NodeIsFolder _) ->
            nodeId |> Id.asFolderId |> Just

        _ ->
            Nothing


getAsDocumentId : Cache.Model -> NodeId -> Maybe DocumentId
getAsDocumentId cache nodeId =
    case Cache.get cache.nodeTypes nodeId of
        Success NodeIsDocument ->
            nodeId |> Id.asDocumentId |> Just

        _ ->
            Nothing


getRootFolder : Cache.Model -> DerivedData ( FolderId, FolderDisplay )
getRootFolder cache =
    cache.rootFolderIds
        |> RemoteData.mapError CacheApiError
        |> RemoteData.andThen
            (\listOfFolderIds ->
                List.head listOfFolderIds
                    |> RemoteData.fromMaybe (CacheDataError "List of root folders is empty")
            )
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
                                    Failure (CacheDataError "Root node is not a folder")
                        )
            )


getParentId : Cache.Model -> FolderId -> ApiData (Maybe FolderId)
getParentId cache id =
    Cache.get cache.folders id
        |> RemoteData.map .parent


getPath : Cache.Model -> FolderId -> ApiData (List FolderId)
getPath cache id =
    getParentId cache id
        |> RemoteData.andThen
            (Maybe.Extra.unwrap
                (RemoteData.Success [ id ])
                (getPath cache
                    >> RemoteData.map ((::) id)
                )
            )


getPathAsFarAsCached : Cache.Model -> Maybe FolderId -> List FolderId
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


isOnPath : Cache.Model -> FolderId -> Maybe FolderId -> Bool
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
