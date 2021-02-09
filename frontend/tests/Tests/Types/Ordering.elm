module Tests.Types.Ordering exposing (suite)

import Cache
import Expect
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types exposing (..)
import Types
import Types.Id as Id
import Types.Selection as Selection


suite : Test
suite =
    describe "Ordering"
        [ testFineOrderingProperties "NodeId" fuzzerNodeId Id.ordering
        , testFineOrderingProperties "FolderId" fuzzerFolderId Id.ordering
        , testFineOrderingProperties "DocumentId" fuzzerDocumentId Id.ordering
        , testFineOrderingProperties "Selection" fuzzerSelection Selection.orderingSelection
        , testFineOrderingProperties "DocumentIdFromSearch" fuzzerDocumentIdFromSearch Types.orderingDocumentIdFromSearch
        , testFineOrderingProperties "SelectionWindow" fuzzerSelectionWindow Cache.orderingSelectionWindow
        , testCoarseOrderingProperties "SelectionFacets" fuzzerSelectionFacets Cache.orderingSelectionFacets
        , testFineOrderingProperties "SearchMethod" fuzzerGlobalFts Selection.orderingGlobalFts
        , testFineOrderingProperties "Sorting" fuzzerSorting Selection.orderingSorting
        , testFineOrderingProperties "FtsFilters" fuzzerFtsFilters Selection.orderingFtsFilters
        , testFineOrderingProperties "FacetFilters" fuzzerFacetFilters Selection.orderingFacetFilters
        , testFineOrderingProperties "Window" fuzzerWindow Types.orderingWindow
        , testOrderingSelectionModuloSorting
        ]


testOrderingSelectionModuloSorting : Test
testOrderingSelectionModuloSorting =
    describe "Ordering Selection modulo sorting"
        [ testCoarseOrderingProperties "Selection modulo sorting" fuzzerSelection Selection.orderingSelectionModuloSorting
        , fuzz fuzzerSelection
            "selections that only differ in FtsSorting should be ragarded as equal"
          <|
            \selection ->
                Selection.orderingSelectionModuloSorting
                    { selection | sorting = Selection.ByRank }
                    { selection | sorting = Selection.ByDate }
                    |> Expect.equal EQ
        ]
