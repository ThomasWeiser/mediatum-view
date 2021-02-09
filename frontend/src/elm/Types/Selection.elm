module Types.Selection exposing
    ( Selection
    , GlobalFts
    , FtsFilter, FtsFilters, initFtsFilters, ftsFiltersFromList
    , FacetFilter, FacetFilters, initFacetFilters
    , Sorting(..)
    , orderingSelection
    , orderingSelectionModuloSorting
    , orderingGlobalFts
    , orderingFtsFilters
    , orderingFacetFilters
    , orderingSorting
    )

{-| A `Selection` is a specification of the possible parameters when querying a set of documents.

@docs Selection
@docs GlobalFts
@docs FtsFilter, FtsFilters, initFtsFilters, ftsFiltersFromList
@docs FacetFilter, FacetFilters, initFacetFilters
@docs Sorting


# Orderings

Define orderings on these types so we can use them as keys in `Sort.Dict`.

@docs orderingSelection
@docs orderingSelectionModuloSorting
@docs orderingGlobalFts
@docs orderingFtsFilters
@docs orderingFacetFilters
@docs orderingSorting

-}

import Ordering exposing (Ordering)
import Types.Aspect exposing (Aspect)
import Types.FilterList as FilterList exposing (FilterList)
import Types.Id as Id exposing (FolderId)
import Types.SearchTerm as SearchTerm exposing (SearchTerm)
import Utils


{-| -}
type alias Selection =
    { scope : FolderId
    , globalFts : GlobalFts
    , ftsFilters : FtsFilters
    , facetFilters : FacetFilters
    , sorting : Sorting
    }


{-| -}
type alias GlobalFts =
    Maybe SearchTerm


{-| -}
type Sorting
    = ByRank
    | ByDate


{-| -}
type alias FacetFilter =
    ( Aspect, String )


{-| A set of facet filters, mapping aspect names to aspect values.
-}
type alias FacetFilters =
    FilterList String


{-| -}
initFacetFilters : FacetFilters
initFacetFilters =
    FilterList.init


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
            (Ordering.byFieldWith orderingGlobalFts .globalFts)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFtsFilters .ftsFilters)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFacetFilters .facetFilters)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingSorting .sorting)


{-| -}
orderingSelectionModuloSorting : Ordering Selection
orderingSelectionModuloSorting =
    Ordering.byFieldWith Id.ordering .scope
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingGlobalFts .globalFts)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFtsFilters .ftsFilters)
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith orderingFacetFilters .facetFilters)


{-| -}
orderingGlobalFts : Ordering (Maybe SearchTerm)
orderingGlobalFts =
    Utils.maybeOrdering SearchTerm.ordering


{-| -}
orderingSorting : Ordering Sorting
orderingSorting =
    Ordering.explicit
        [ ByRank, ByDate ]


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
