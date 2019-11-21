module Types.Selection exposing
    ( Filter(..)
    , Filters
    , FtsSorting(..)
    , SelectMethod(..)
    , Selection
    , filterHandle
    , filtersNone
    , filtersToList
    , insertFilter
    , orderingFilter
    , orderingFilters
    , orderingFtsSorting
    , orderingSearchMethod
    , orderingSelection
    , removeFilter
    )

import Dict
import Ordering exposing (Ordering)
import Range exposing (Range)
import Types.Id as Id exposing (FolderId)
import Types.SearchTerm as SearchTerm exposing (SearchTerm)
import Utils


type alias Selection =
    { scope : FolderId
    , selectMethod : SelectMethod
    , filters : Filters
    }


type SelectMethod
    = SelectByFolderListing
    | SelectByFullTextSearch SearchTerm FtsSorting


type FtsSorting
    = FtsByRank
    | FtsByDate


type alias Filters =
    Dict.Dict String Filter


type Filter
    = FilterYearWithin (Range Int)
    | FilterTitleFts SearchTerm


filtersNone : Filters
filtersNone =
    Dict.empty


insertFilter : Filter -> Filters -> Filters
insertFilter filter filters =
    Dict.insert (filterHandle filter) filter filters


removeFilter : String -> Filters -> Filters
removeFilter handle filters =
    Dict.remove handle filters


filtersToList : Filters -> List Filter
filtersToList filters =
    Dict.values filters


filterHandle : Filter -> String
filterHandle filter =
    case filter of
        FilterYearWithin _ ->
            "YearWithin"

        FilterTitleFts searchTerm ->
            "TitleFts-" ++ SearchTerm.toString searchTerm


orderingSelection : Ordering Selection
orderingSelection =
    Ordering.byFieldWith Id.ordering .scope
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingSearchMethod .selectMethod)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFilters .filters)


orderingSearchMethod : Ordering SelectMethod
orderingSearchMethod =
    Ordering.byRank
        (\selectMethod ->
            case selectMethod of
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
                    SearchTerm.ordering searchTermL searchTermR
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
                    SearchTerm.ordering sL sR

                _ ->
                    Ordering.noConflicts
        )
