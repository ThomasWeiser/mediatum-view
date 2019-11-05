module Data.Utils exposing
    ( filtersNone
    , folderCountsFromList
    , folderCountsInit
    , nodeIdToString
    , setOfSearchTermsFromList
    , setOfSearchTermsInit
    )

import Data.Ordering
import Data.Types exposing (..)
import Dict
import Sort.Dict
import Sort.Set
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


setOfSearchTermsInit : Sort.Set.Set SearchTerm
setOfSearchTermsInit =
    Sort.Set.empty
        (Data.Ordering.sorter Data.Ordering.orderingSearchTerm)


setOfSearchTermsFromList : List SearchTerm -> Sort.Set.Set SearchTerm
setOfSearchTermsFromList listOfSearchTerms =
    Sort.Set.fromList
        (Data.Ordering.sorter Data.Ordering.orderingSearchTerm)
        listOfSearchTerms


nodeIdToString : NodeId -> String
nodeIdToString nodeId =
    nodeId |> nodeIdToInt |> String.fromInt
