module Tests.Types.Ordering exposing (suite)

import Cache
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types exposing (..)
import Types
import Types.Id as Id
import Types.Selection as Selection


suite : Test
suite =
    describe "Data.Ordering"
        [ testOrderingProperties "NodeId" fuzzerNodeId Id.ordering
        , testOrderingProperties "FolderId" fuzzerFolderId Id.ordering
        , testOrderingProperties "DocumentId" fuzzerDocumentId Id.ordering
        , testOrderingProperties "Selection" fuzzerSelection Selection.orderingSelection
        , testOrderingProperties "SelectionWindow" fuzzerSelectionWindow Cache.orderingSelectionWindow
        , testOrderingProperties "SearchMethod" fuzzerSearchMethod Selection.orderingSearchMethod
        , testOrderingProperties "FtsSorting" fuzzerFtsSorting Selection.orderingFtsSorting
        , testOrderingProperties "Filters" fuzzerFilters Selection.orderingFilters
        , testOrderingProperties "Filter" fuzzerFilter Selection.orderingFilter
        , testOrderingProperties "Window" fuzzerWindow Types.orderingWindow
        ]
