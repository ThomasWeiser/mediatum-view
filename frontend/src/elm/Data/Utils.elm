module Data.Utils exposing (filtersNone, folderCountsFromList, folderCountsInit)

import Data.Ordering
import Data.Types exposing (..)
import Dict
import Sort.Dict


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
