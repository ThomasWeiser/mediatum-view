module Data.Utils exposing
    ( cleanSearchTerm
    , filtersNone
    , folderCountsFromList
    , folderCountsInit
    , nodeIdToString
    )

import Data.Ordering
import Data.Types exposing (..)
import Dict
import Sort.Dict
import String.Extra


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


cleanSearchTerm : String -> String
cleanSearchTerm =
    -- Trim the whitespace of both sides of the string
    -- and compress repeated whitespace internally to a single whitespace char.
    String.Extra.clean


nodeIdToString : NodeId -> String
nodeIdToString nodeId =
    nodeId |> nodeIdToInt |> String.fromInt
