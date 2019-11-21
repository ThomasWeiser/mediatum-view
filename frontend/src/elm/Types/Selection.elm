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
    , filters : SetOfFilters
    }


type SelectMethod
    = SelectByFolderListing
    | SelectByFullTextSearch SearchTerm FtsSorting


type FtsSorting
    = FtsByRank
    | FtsByDate


type SetOfFilters
    = SetOfFilters (Dict.Dict FilterHandle Filter)


type Filter
    = FilterYearWithin (Range Int)
    | FilterTitleFts SearchTerm


type alias FilterHandle =
    ( String, Float )


filtersNone : SetOfFilters
filtersNone =
    SetOfFilters Dict.empty


insertFilter : Filter -> SetOfFilters -> SetOfFilters
insertFilter filter (SetOfFilters fs) =
    SetOfFilters (Dict.insert (filterHandle filter) filter fs)


removeFilter : FilterHandle -> SetOfFilters -> SetOfFilters
removeFilter handle (SetOfFilters fs) =
    SetOfFilters (Dict.remove handle fs)


filtersToList : SetOfFilters -> List Filter
filtersToList (SetOfFilters fs) =
    Dict.values fs


filtersFromList : List Filter -> SetOfFilters
filtersFromList listOfFilters =
    listOfFilters
        |> List.map (\filter -> ( filterHandle filter, filter ))
        |> Dict.fromList
        |> SetOfFilters


filterHandle : Filter -> FilterHandle
filterHandle filter =
    ( case filter of
        FilterYearWithin _ ->
            "YearWithin"

        FilterTitleFts searchTerm ->
            "TitleFts-"
                ++ SearchTerm.toString searchTerm
    , 0.0
    )


newFilterHandle : String -> FilterHandle
newFilterHandle filterTypeName =
    ( "new-" ++ filterTypeName
    , 0.0
    )


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
        (Utils.lexicalOrder orderingStringFilter)
        (\(SetOfFilters fs1) -> Dict.toList fs1)


orderingStringFilter : Ordering ( FilterHandle, Filter )
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
