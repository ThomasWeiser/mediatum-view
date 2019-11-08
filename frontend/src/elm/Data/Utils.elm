module Data.Utils exposing
    ( filtersNone
    , folderCountsFromList
    , folderCountsInit
    )

import Data.Ordering
import Dict
import Sort.Dict
import Types exposing (..)
import Types.FolderId as FolderId exposing (FolderId)


filtersNone : Filters
filtersNone =
    Dict.empty


folderCountsInit : FolderCounts
folderCountsInit =
    Sort.Dict.empty
        (Data.Ordering.sorter FolderId.ordering)


folderCountsFromList : List ( FolderId, Int ) -> FolderCounts
folderCountsFromList listOfPairs =
    Sort.Dict.fromList
        (Data.Ordering.sorter FolderId.ordering)
        listOfPairs
