module Tests.Types exposing
    ( fuzzerDocumentId
    , fuzzerFilter
    , fuzzerFilters
    , fuzzerFolderId
    , fuzzerFtsSorting
    , fuzzerLimit
    , fuzzerNodeId
    , fuzzerOffset
    , fuzzerSearchMethod
    , fuzzerSelection
    , fuzzerSelectionWindow
    , fuzzerWindow
    , fuzzerYear
    )

import Basics.Extra
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import String.Extra
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Range
import Tests.Types.SearchTerm exposing (fuzzerSearchTerm)
import Types exposing (..)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (Filter(..), Filters, FtsSorting(..), SearchMethod(..), Selection)
import Types.Window as Window exposing (Window)


fuzzerId : Fuzzer Int
fuzzerId =
    Fuzz.oneOf
        [ Fuzz.intRange 0 2
        , Fuzz.intRange 0 5
        , Fuzz.intRange 0 10
        , Fuzz.intRange 0 999999
        ]


fuzzerFolderId : Fuzzer FolderId
fuzzerFolderId =
    Fuzz.map Id.fromInt fuzzerId


fuzzerNodeId : Fuzzer NodeId
fuzzerNodeId =
    Fuzz.map Id.fromInt fuzzerId


fuzzerDocumentId : Fuzzer DocumentId
fuzzerDocumentId =
    Fuzz.map Id.fromInt fuzzerId


fuzzerSelection : Fuzzer Selection
fuzzerSelection =
    Fuzz.map3 Selection
        fuzzerFolderId
        fuzzerSearchMethod
        fuzzerFilters


fuzzerSelectionWindow : Fuzzer ( Selection, Window )
fuzzerSelectionWindow =
    Fuzz.map2 Tuple.pair
        fuzzerSelection
        fuzzerWindow


fuzzerSearchMethod : Fuzzer SearchMethod
fuzzerSearchMethod =
    Fuzz.frequency
        [ ( 1, Fuzz.constant SelectByFolderListing )
        , ( 5
          , Fuzz.map2
                SelectByFullTextSearch
                fuzzerSearchTerm
                fuzzerFtsSorting
          )
        ]


fuzzerFtsSorting : Fuzzer FtsSorting
fuzzerFtsSorting =
    Fuzz.oneOf
        [ Fuzz.constant FtsByRank
        , Fuzz.constant FtsByDate
        ]


fuzzerFilters : Fuzzer Filters
fuzzerFilters =
    Fuzz.map Dict.fromList <|
        shortList 3 <|
            Fuzz.map2 Tuple.pair
                Fuzz.string
                fuzzerFilter


fuzzerFilter : Fuzzer Filter
fuzzerFilter =
    Fuzz.oneOf
        [ Fuzz.map
            FilterYearWithin
            (Tests.Range.fuzzerRange fuzzerYear)
        , Fuzz.map
            FilterTitleFts
            fuzzerSearchTerm
        ]


fuzzerWindow : Fuzzer Window
fuzzerWindow =
    Fuzz.map2 Window
        fuzzerOffset
        fuzzerLimit


fuzzerOffset : Fuzzer Int
fuzzerOffset =
    Fuzz.frequency
        [ ( 100, Fuzz.constant 0 )
        , ( 5, Fuzz.constant 5 )
        , ( 50, Fuzz.constant 10 )
        , ( 10, Fuzz.constant 20 )
        , ( 5, Fuzz.constant 50 )
        , ( 30, Fuzz.intRange 0 200 )
        ]


fuzzerLimit : Fuzzer Int
fuzzerLimit =
    Fuzz.frequency
        [ ( 10, Fuzz.constant 0 )
        , ( 5, Fuzz.constant 5 )
        , ( 50, Fuzz.constant 10 )
        , ( 10, Fuzz.constant 20 )
        , ( 5, Fuzz.constant 50 )
        , ( 30, Fuzz.intRange 0 200 )
        ]


{-| A fuzzer for numbers representing years,
favoring years from 1960 to 2040,
but also generating random years between 0 and 99999.
-}
fuzzerYear : Fuzzer Int
fuzzerYear =
    Fuzz.frequency
        [ ( 1, Fuzz.intRange 0 99999 )
        , ( 5, Fuzz.intRange 1960 2040 )
        ]
