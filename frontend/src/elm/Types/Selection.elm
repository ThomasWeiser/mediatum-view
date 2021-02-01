module Types.Selection exposing
    ( Selection
    , SelectMethod(..)
    , FtsSorting(..)
    , FacetFilters, initFacetFilters, facetFiltersFromList
    , FtsFilter, FtsFilters, initFtsFilters, ftsFiltersFromList
    , orderingSelection
    , orderingSelectionModuloSorting
    , orderingSelectMethod
    , orderingFtsSorting
    , orderingFtsFilters
    , orderingFacetFilters
    )

{-| A `Selection` is a specification of the possible parameters when querying a set of documents.

@docs Selection
@docs SelectMethod
@docs FtsSorting
@docs FacetFilters, initFacetFilters, facetFiltersFromList
@docs FtsFilter, FtsFilters, initFtsFilters, ftsFiltersFromList


# Orderings

Define orderings on these types so we can use them as keys in `Sort.Dict`.

@docs orderingSelection
@docs orderingSelectionModuloSorting
@docs orderingSelectMethod
@docs orderingFtsSorting
@docs orderingFtsFilters
@docs orderingFacetFilters

-}

import Dict
import Ordering exposing (Ordering)
import Sort.Dict
import Types.Aspect as Aspect exposing (Aspect)
import Types.FilterList as FilterList exposing (FilterList)
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
    FilterList String


{-| -}
initFacetFilters : FacetFilters
initFacetFilters =
    FilterList.init


{-| -}
facetFiltersFromList : List ( Aspect, String ) -> FacetFilters
facetFiltersFromList =
    FilterList.fromList


{-| -}
type alias FtsFilter =
    ( Aspect, SearchTerm )


{-| -}
type alias FtsFilters =
    FilterList SearchTerm


{-| -}
initFtsFilters : FtsFilters
initFtsFilters =
    FilterList.init


{-| -}
ftsFiltersFromList : List FtsFilter -> FtsFilters
ftsFiltersFromList =
    FilterList.fromList


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
    FilterList.ordering
        SearchTerm.ordering


{-| -}
orderingFacetFilters : Ordering FacetFilters
orderingFacetFilters =
    FilterList.ordering
        Ordering.natural
