module Types.Selection exposing
    ( Filter(..)
    , FilterHandle
    , FtsSorting(..)
    , SelectMethod(..)
    , Selection
    , SetOfFilters
    , filterHandle
    , filtersFromList
    , filtersNone
    , filtersToList
    , insertFilter
    , newFilterHandle
    , orderingFilter
    , orderingFilterHandle
    , orderingFilters
    , orderingFtsSorting
    , orderingSearchMethod
    , orderingSelection
    , removeFilter
    )

import Ordering exposing (Ordering)
import Sort.Dict
import Types.Id as Id exposing (FolderId)
import Types.Range as Range exposing (Range)
import Types.SearchTerm as SearchTerm exposing (SearchTerm)
import Utils


type alias Selection =
    { scope : FolderId
    , selectMethod : SelectMethod
    , filters : SetOfFilters
    }


type SelectMethod
    = SelectByFolderListing
    | SelectByFullTextSearch SearchTerm FtsSorting


type FtsSorting
    = FtsByRank
    | FtsByDate


type SetOfFilters
    = SetOfFilters (Sort.Dict.Dict FilterHandle Filter)


type Filter
    = FilterYearWithin (Range Int)
    | FilterTitleFts SearchTerm


type FilterHandle
    = FilterHandle String


filtersNone : SetOfFilters
filtersNone =
    SetOfFilters (Sort.Dict.empty (Utils.sorter orderingFilterHandle))


insertFilter : Filter -> SetOfFilters -> SetOfFilters
insertFilter filter (SetOfFilters fs) =
    SetOfFilters (Sort.Dict.insert (filterHandle filter) filter fs)


removeFilter : FilterHandle -> SetOfFilters -> SetOfFilters
removeFilter handle (SetOfFilters fs) =
    SetOfFilters (Sort.Dict.remove handle fs)


filtersToList : SetOfFilters -> List Filter
filtersToList (SetOfFilters fs) =
    Sort.Dict.values fs


filtersFromList : List Filter -> SetOfFilters
filtersFromList listOfFilters =
    listOfFilters
        |> List.map (\filter -> ( filterHandle filter, filter ))
        |> Sort.Dict.fromList (Utils.sorter orderingFilterHandle)
        |> SetOfFilters


filterHandle : Filter -> FilterHandle
filterHandle filter =
    FilterHandle <|
        case filter of
            FilterYearWithin _ ->
                "YearWithin"

            FilterTitleFts searchTerm ->
                "TitleFts-"
                    ++ SearchTerm.toString searchTerm


newFilterHandle : String -> FilterHandle
newFilterHandle filterTypeName =
    FilterHandle ("new-" ++ filterTypeName)


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


orderingFilters : Ordering SetOfFilters
orderingFilters =
    Ordering.byFieldWith
        (Utils.lexicalOrder orderingTupleOfFilterHandleAndFilter)
        (\(SetOfFilters fs1) -> Sort.Dict.toList fs1)


orderingTupleOfFilterHandleAndFilter : Ordering ( FilterHandle, Filter )
orderingTupleOfFilterHandleAndFilter =
    Ordering.byFieldWith orderingFilterHandle Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFilter Tuple.second)


orderingFilterHandle : Ordering FilterHandle
orderingFilterHandle (FilterHandle h1) (FilterHandle h2) =
    compare h1 h2


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
