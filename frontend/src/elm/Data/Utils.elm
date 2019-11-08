module Data.Utils exposing
    ( filtersNone
    , folderCountsFromList
    , folderCountsInit
    , nodeIdToString
    )

import Data.Ordering
import Dict
import Sort.Dict
import Types exposing (..)


filtersNone : Filters
filtersNone =
    Dict.empty


folderCountsInit : FolderCounts
folderCountsInit =
    Sort.Dict.empty
        (Data.Ordering.sorter Data.Ordering.orderingFolderId)


folderCountsFromList : List ( FolderId, Int ) -> FolderCounts
folderCountsFromList listOfPairs =
    Sort.Dict.fromList
        (Data.Ordering.sorter Data.Ordering.orderingFolderId)
        listOfPairs


nodeIdToString : NodeId -> String
nodeIdToString nodeId =
    nodeId |> nodeIdToInt |> String.fromInt
