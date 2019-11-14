module Tests.Data.Ordering exposing (suite)

import Data.Ordering exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types exposing (..)
import Types exposing (..)
import Types.DocumentId as DocumentId
import Types.FolderId as FolderId
import Types.NodeId as NodeId


suite : Test
suite =
    describe "Data.Ordering"
        [ testOrderingProperties "NodeId" fuzzerNodeId NodeId.ordering
        , testOrderingProperties "FolderId" fuzzerFolderId FolderId.ordering
        , testOrderingProperties "DocumentId" fuzzerDocumentId DocumentId.ordering
        , testOrderingProperties "Selection" fuzzerSelection orderingSelection
        , testOrderingProperties "SelectionWindow" fuzzerSelectionWindow orderingSelectionWindow
        , testOrderingProperties "SearchMethod" fuzzerSearchMethod orderingSearchMethod
        , testOrderingProperties "FtsSorting" fuzzerFtsSorting orderingFtsSorting
        , testOrderingProperties "Filters" fuzzerFilters orderingFilters
        , testOrderingProperties "Filter" fuzzerFilter orderingFilter
        , testOrderingProperties "Window" fuzzerWindow orderingWindow
        ]
