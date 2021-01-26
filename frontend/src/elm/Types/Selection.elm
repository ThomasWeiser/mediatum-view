module Types.Selection exposing
    ( Selection
    , SelectMethod(..)
    , FtsSorting(..)
    , SetOfFilters
    , Filter(..)
    , FilterHandle
    , FacetFilters, initFacetFilters, facetFiltersFromList
    , FtsFilter, FtsFilters, initFtsFilters, ftsFiltersFromList
    , filtersNone
    , insertFilter
    , removeFilter
    , filtersToList
    , filtersFromList
    , filterHandle
    , newFilterHandle
    , orderingSelection
    , orderingSelectionModuloSorting
    , orderingSelectMethod
    , orderingFtsSorting
    , orderingFtsFilters
    , orderingFacetFilters
    , orderingFilterHandle
    )

{-| A `Selection` is a specification of the possible parameters when querying a set of documents.

@docs Selection
@docs SelectMethod
@docs FtsSorting
@docs SetOfFilters
@docs Filter
@docs FilterHandle
@docs FacetFilters, initFacetFilters, facetFiltersFromList
@docs FtsFilter, FtsFilters, initFtsFilters, ftsFiltersFromList

@docs filtersNone
@docs insertFilter
@docs removeFilter
@docs filtersToList
@docs filtersFromList
@docs filterHandle
@docs newFilterHandle


# Orderings

Define orderings on these types so we can use them as keys in `Sort.Dict`.

@docs orderingSelection
@docs orderingSelectionModuloSorting
@docs orderingSelectMethod
@docs orderingFtsSorting
@docs orderingFtsFilters
@docs orderingFacetFilters
@docs orderingFilterHandle

-}

import Dict
import Ordering exposing (Ordering)
import Sort.Dict
import Types.Aspect as Aspect exposing (Aspect)
import Types.Id as Id exposing (FolderId)
import Types.Range as Range exposing (Range)
import Types.SearchTerm as SearchTerm exposing (SearchTerm)
import Utils


{-| -}
type alias Selection =
    { scope : FolderId
    , selectMethod : SelectMethod
    , ftsFilters : FtsFilters
    , facetFilters : FacetFilters
    }


{-| Do we want a full-text search or do we want to list a directory of documents?
-}
type SelectMethod
    = SelectByFolderListing
    | SelectByFullTextSearch SearchTerm FtsSorting


{-| -}
type FtsSorting
    = FtsByRank
    | FtsByDate


{-| A set of facet filters, mapping aspect names to aspect values.
-}
type alias FacetFilters =
    Sort.Dict.Dict Aspect String


{-| -}
initFacetFilters : FacetFilters
initFacetFilters =
    Sort.Dict.empty (Utils.sorter Aspect.ordering)


{-| -}
facetFiltersFromList : List ( Aspect, String ) -> FacetFilters
facetFiltersFromList list =
    list
        |> Sort.Dict.fromList (Utils.sorter Aspect.ordering)


{-| -}
type alias FtsFilter =
    ( Aspect, SearchTerm )


{-| -}
type alias FtsFilters =
    Sort.Dict.Dict Aspect SearchTerm


{-| -}
initFtsFilters : FtsFilters
initFtsFilters =
    Sort.Dict.empty (Utils.sorter Aspect.ordering)


{-| -}
ftsFiltersFromList : List FtsFilter -> FtsFilters
ftsFiltersFromList list =
    list
        |> Sort.Dict.fromList (Utils.sorter Aspect.ordering)


{-| A `SetOfFilters` may contain one single `FilterYearWithin` and one single `FilterTitleFts`.
-}
type SetOfFilters
    = SetOfFilters (Sort.Dict.Dict FilterHandle Filter)


{-| -}
type Filter
    = FilterYearWithin (Range Int)
    | FilterTitleFts SearchTerm


{-| A `FilterHandle` is a wrapped string used to reference a filter instance in the UI as well as in a `SetOfFilters`.
-}
type FilterHandle
    = FilterHandle String


{-| -}
filtersNone : SetOfFilters
filtersNone =
    SetOfFilters (Sort.Dict.empty (Utils.sorter orderingFilterHandle))


{-| -}
insertFilter : Filter -> SetOfFilters -> SetOfFilters
insertFilter filter (SetOfFilters fs) =
    SetOfFilters (Sort.Dict.insert (filterHandle filter) filter fs)


{-| -}
removeFilter : FilterHandle -> SetOfFilters -> SetOfFilters
removeFilter handle (SetOfFilters fs) =
    SetOfFilters (Sort.Dict.remove handle fs)


{-| -}
filtersToList : SetOfFilters -> List Filter
filtersToList (SetOfFilters fs) =
    Sort.Dict.values fs


{-| -}
filtersFromList : List Filter -> SetOfFilters
filtersFromList listOfFilters =
    listOfFilters
        |> List.map (\filter -> ( filterHandle filter, filter ))
        |> Sort.Dict.fromList (Utils.sorter orderingFilterHandle)
        |> SetOfFilters


{-| -}
filterHandle : Filter -> FilterHandle
filterHandle filter =
    FilterHandle <|
        case filter of
            FilterYearWithin _ ->
                "YearWithin"

            FilterTitleFts searchTerm ->
                "TitleFts"


{-| Used for newly created filter editors.
-}
newFilterHandle : String -> FilterHandle
newFilterHandle filterTypeName =
    FilterHandle ("new-" ++ filterTypeName)


{-| -}
orderingSelection : Ordering Selection
orderingSelection =
    Ordering.byFieldWith Id.ordering .scope
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingSelectMethod .selectMethod)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFtsFilters .ftsFilters)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFacetFilters .facetFilters)


{-| -}
orderingSelectionModuloSorting : Ordering Selection
orderingSelectionModuloSorting =
    Ordering.byFieldWith Id.ordering .scope
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingSelectMethodModuloSorting .selectMethod)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFtsFilters .ftsFilters)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFacetFilters .facetFilters)


{-| -}
orderingSelectMethod : Ordering SelectMethod
orderingSelectMethod =
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


{-| -}
orderingSelectMethodModuloSorting : Ordering SelectMethod
orderingSelectMethodModuloSorting =
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

                _ ->
                    Ordering.noConflicts
        )


{-| -}
orderingFtsSorting : Ordering FtsSorting
orderingFtsSorting =
    Ordering.explicit
        [ FtsByRank, FtsByDate ]


{-| -}
orderingFtsFilters : Ordering FtsFilters
orderingFtsFilters =
    Ordering.byFieldWith
        (Utils.lexicalOrdering
            (Utils.tupleOrdering
                Aspect.ordering
                SearchTerm.ordering
            )
        )
        Sort.Dict.toList


{-| -}



-- TODO Remove


orderingFilterHandle : Ordering FilterHandle
orderingFilterHandle (FilterHandle h1) (FilterHandle h2) =
    compare h1 h2


{-| -}
orderingFacetFilters : Ordering FacetFilters
orderingFacetFilters =
    Ordering.byFieldWith
        (Utils.lexicalOrdering
            (Utils.tupleOrdering
                Aspect.ordering
                Ordering.natural
            )
        )
        Sort.Dict.toList
