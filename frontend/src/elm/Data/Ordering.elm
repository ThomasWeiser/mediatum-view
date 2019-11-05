module Data.Ordering exposing
    ( orderingDocumentId
    , orderingFilter
    , orderingFilters
    , orderingFolderId
    , orderingFtsSorting
    , orderingNodeId
    , orderingSearchMethod
    , orderingSearchTerm
    , orderingSelection
    , orderingSelectionWindow
    , orderingStringFilter
    , orderingWindow
    , sorter
    )

import Data.Types exposing (..)
import Dict
import Ordering exposing (..)
import Range exposing (Range)
import Sort exposing (Sorter)
import Utils


sorter : Ordering a -> Sorter a
sorter ordering =
    Sort.custom ordering


orderingNodeId : Ordering NodeId
orderingNodeId =
    Ordering.byField nodeIdToInt


orderingFolderId : Ordering FolderId
orderingFolderId =
    Ordering.byField folderIdToInt


orderingDocumentId : Ordering DocumentId
orderingDocumentId =
    Ordering.byField documentIdToInt


orderingSelection : Ordering Selection
orderingSelection =
    Ordering.byFieldWith orderingFolderId .scope
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingSearchMethod .searchMethod)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFilters .filters)


orderingSelectionWindow : Ordering ( Selection, Window )
orderingSelectionWindow =
    Ordering.byFieldWith orderingSelection Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingWindow Tuple.second)


orderingSearchMethod : Ordering SearchMethod
orderingSearchMethod =
    Ordering.byRank
        (\searchMethod ->
            case searchMethod of
                SelectByFolderListing ->
                    1

                SelectByFullTextSearch _ _ ->
                    2
        )
        (\searchMethodL searchMethodR ->
            case
                ( searchMethodL, searchMethodR )
            of
                ( SelectByFullTextSearch searchTermL ftsSortingL, SelectByFullTextSearch searchTermR ftsSortingR ) ->
                    orderingSearchTerm searchTermL searchTermR
                        |> Ordering.ifStillTiedThen
                            (orderingFtsSorting ftsSortingL ftsSortingR)

                _ ->
                    Ordering.noConflicts
        )


orderingFtsSorting : Ordering FtsSorting
orderingFtsSorting =
    Ordering.explicit
        [ FtsByRank, FtsByDate ]


orderingFilters : Ordering Filters
orderingFilters =
    Ordering.byFieldWith
        (Utils.lexicalOrder orderingStringFilter)
        Dict.toList


orderingStringFilter : Ordering ( String, Filter )
orderingStringFilter =
    Ordering.byField Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFilter Tuple.second)


orderingFilter : Ordering Filter
orderingFilter =
    Ordering.byRank
        (\filter ->
            case filter of
                FilterYearWithin _ ->
                    1

                FilterTitleFts _ ->
                    2
        )
        (\filterL filterR ->
            case
                ( filterL, filterR )
            of
                ( FilterYearWithin range1, FilterYearWithin range2 ) ->
                    Range.compare range1 range2

                ( FilterTitleFts sL, FilterTitleFts sR ) ->
                    orderingSearchTerm sL sR

                _ ->
                    Ordering.noConflicts
        )


orderingWindow : Ordering Window
orderingWindow =
    Ordering.byField .offset
        |> Ordering.breakTiesWith
            (Ordering.byField .limit)


orderingSearchTerm : Ordering SearchTerm
orderingSearchTerm =
    Ordering.byField searchTermToString
