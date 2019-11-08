module Data.Derive exposing
    ( getParentId
    , getPath
    , getPathAsFarAsCached
    , isOnPath
    )

import Data.Cache as Cache exposing (ApiData)
import Maybe.Extra
import RemoteData
import Types.FolderId as FolderId exposing (FolderId)


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
