module Tests.Types.Ordering exposing (suite)

import Cache
import Expect
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types exposing (..)
import Tests.Types.SearchTerm exposing (fuzzerSearchTerm)
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
        , testFineOrderingProperties "SearchMethod" fuzzerSearchMethod Selection.orderingSelectMethod
        , testFineOrderingProperties "FtsSorting" fuzzerFtsSorting Selection.orderingFtsSorting
        , testFineOrderingProperties "FtsFilters" fuzzerFtsFilters Selection.orderingFtsFilters
        , testFineOrderingProperties "FacetFilters" fuzzerFacetFilters Selection.orderingFacetFilters
        , testFineOrderingProperties "Window" fuzzerWindow Types.orderingWindow
        , testOrderingSelectionModuloSorting
        ]


testOrderingSelectionModuloSorting : Test
testOrderingSelectionModuloSorting =
    describe "Ordering Selection modulo sorting"
        [ testCoarseOrderingProperties "Selection modulo sorting" fuzzerSelection Selection.orderingSelectionModuloSorting
        , fuzz2
            fuzzerSelection
            fuzzerSearchTerm
            "selections that only differ in FtsSorting should be ragarded as equal"
          <|
            \selection searchTerm ->
                Selection.orderingSelectionModuloSorting
                    { selection
                        | selectMethod =
                            Selection.SelectByFullTextSearch
                                searchTerm
                                Selection.FtsByRank
                    }
                    { selection
                        | selectMethod =
                            Selection.SelectByFullTextSearch
                                searchTerm
                                Selection.FtsByDate
                    }
                    |> Expect.equal EQ
        ]
