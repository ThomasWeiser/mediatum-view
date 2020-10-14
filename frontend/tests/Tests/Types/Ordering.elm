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
        [ testOrderingProperties "NodeId" fuzzerNodeId Id.ordering
        , testOrderingProperties "FolderId" fuzzerFolderId Id.ordering
        , testOrderingProperties "DocumentId" fuzzerDocumentId Id.ordering
        , testOrderingProperties "Selection" fuzzerSelection Selection.orderingSelection
        , testOrderingProperties "SelectionWindow" fuzzerSelectionWindow Cache.orderingSelectionWindow
        , testOrderingProperties "SearchMethod" fuzzerSearchMethod Selection.orderingSelectMethod
        , testOrderingProperties "FtsSorting" fuzzerFtsSorting Selection.orderingFtsSorting
        , testOrderingProperties "Filters" fuzzerFilters Selection.orderingFilters
        , testOrderingProperties "Filter" fuzzerFilter Selection.orderingFilter
        , testOrderingProperties "FacetFilters" fuzzerFacetFilters Selection.orderingFacetFilters
        , testOrderingProperties "Window" fuzzerWindow Types.orderingWindow
        , testOrderingSelectionModuloSorting
        ]


testOrderingSelectionModuloSorting : Test
testOrderingSelectionModuloSorting =
    describe "Ordering Selection modulo sorting"
        [ testPreorderingProperties "Selection modulo sorting" fuzzerSelection Selection.orderingSelectionModuloSorting
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
