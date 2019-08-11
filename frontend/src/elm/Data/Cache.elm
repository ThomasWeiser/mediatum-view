module Data.Cache exposing (ApiData, Cache, Need(..), Needs)

import Api
import Api.Queries
import Basics.Extra
import Data.Types exposing (..)
import Dict exposing (Dict)
import List.Extra
import RemoteData exposing (RemoteData(..))


type alias ApiData a =
    RemoteData Api.Error a


type alias Cache =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Dict FolderId (ApiData Folder)
    , subFolders : Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Dict Int (ApiData NodeType)
    , folderCounts : Dict Selection (ApiData (Dict FolderId Int))
    , docListPages : Dict Selection (Dict Window (ApiData DocumentsPage))
    , documents : Dict DocumentId (ApiData Document)
    }


type alias Needs =
    -- Poss. use a different aggregation abstraction,
    -- like a recursive Need type or a Set
    List Need


type Need
    = NeedRootFolderIds
    | NeedFolder FolderId



{-
   | NeedSubfolders FolderId
   | NeedNodeType Int
   | NeedFolderCounts Selection
   | NeedDocListPage Selection Window
   | NeedDocument DocumentId
-}


type Msg
    = NoOp
    | ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))


requestNeeds : Needs -> Cache -> ( Cache, Cmd Msg )
requestNeeds needs cache =
    List.Extra.mapAccuml
        (Basics.Extra.flip requestNeed)
        cache
        needs
        |> Tuple.mapSecond Cmd.batch


requestNeed : Need -> Cache -> ( Cache, Cmd Msg )
requestNeed need cache =
    case need of
        NeedRootFolderIds ->
            case cache.rootFolderIds of
                NotAsked ->
                    ( { cache
                        | rootFolderIds = Loading
                      }
                      -- todo
                    , Api.sendQueryRequest
                        ApiResponseToplevelFolder
                        Api.Queries.toplevelFolder
                    )

                _ ->
                    ( cache, Cmd.none )

        NeedFolder folderId ->
            ( cache, Cmd.none )
