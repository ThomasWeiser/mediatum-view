module Data.Utils exposing
    ( filtersNone
    , folderCountsFromList
    , folderCountsInit
    )

import Dict
import Sort.Dict
import Types exposing (..)
import Types.Id as Id exposing (FolderId)
import Types.Ordering


filtersNone : Filters
filtersNone =
    Dict.empty


folderCountsInit : FolderCounts
folderCountsInit =
    Sort.Dict.empty
        (Types.Ordering.sorter Id.ordering)


folderCountsFromList : List ( FolderId, Int ) -> FolderCounts
folderCountsFromList listOfPairs =
    Sort.Dict.fromList
        (Types.Ordering.sorter Id.ordering)
        listOfPairs
