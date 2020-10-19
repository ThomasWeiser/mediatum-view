module Tests.Types exposing
    ( fuzzerDocumentId
    , fuzzerDocumentIdAndMaybeSearchTerm
    , fuzzerFacetFilters
    , fuzzerFilter
    , fuzzerFilters
    , fuzzerFolderId
    , fuzzerFtsSorting
    , fuzzerLimit
    , fuzzerNodeId
    , fuzzerOffset
    , fuzzerSearchMethod
    , fuzzerSelection
    , fuzzerSelectionFacet
    , fuzzerSelectionWindow
    , fuzzerWindow
    , fuzzerYear
    )

import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types.Range
import Tests.Types.SearchTerm exposing (fuzzerSearchTerm)
import Types exposing (Window)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm as SearchTerm exposing (SearchTerm)
import Types.Selection exposing (FacetFilters, Filter(..), FtsSorting(..), SelectMethod(..), Selection, SetOfFilters)


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


fuzzerDocumentIdAndMaybeSearchTerm : Fuzzer ( DocumentId, Maybe SearchTerm )
fuzzerDocumentIdAndMaybeSearchTerm =
    Fuzz.map2 Tuple.pair
        (Fuzz.map Id.fromInt fuzzerId)
        (Fuzz.maybe fuzzerSearchTerm)


fuzzerSelection : Fuzzer Selection
fuzzerSelection =
    Fuzz.map4 Selection
        fuzzerFolderId
        fuzzerSearchMethod
        fuzzerFilters
        fuzzerFacetFilters


fuzzerSelectionWindow : Fuzzer ( Selection, Window )
fuzzerSelectionWindow =
    Fuzz.map2 Tuple.pair
        fuzzerSelection
        fuzzerWindow


fuzzerSelectionFacet : Fuzzer ( Selection, String )
fuzzerSelectionFacet =
    Fuzz.map2 Tuple.pair
        fuzzerSelection
        fuzzerFacet


fuzzerFacet : Fuzzer String
fuzzerFacet =
    Fuzz.oneOf
        [ [ "year"
          , "author"
          ]
            |> List.map Fuzz.constant
            |> Fuzz.oneOf
        , Fuzz.string
        ]


fuzzerSearchMethod : Fuzzer SelectMethod
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


fuzzerFilters : Fuzzer SetOfFilters
fuzzerFilters =
    fuzzerFilter
        |> shortList 3
        |> Fuzz.map Types.Selection.filtersFromList


fuzzerFilter : Fuzzer Filter
fuzzerFilter =
    Fuzz.oneOf
        [ Fuzz.map
            FilterYearWithin
            (Tests.Types.Range.fuzzerRange fuzzerYear)
        , Fuzz.map
            FilterTitleFts
            fuzzerSearchTerm
        ]


fuzzerFacetFilters : Fuzzer FacetFilters
fuzzerFacetFilters =
    Fuzz.map2 Tuple.pair
        fuzzerFacetKey
        Fuzz.string
        |> shortList 3
        |> Fuzz.map Dict.fromList


fuzzerFacetKey : Fuzzer String
fuzzerFacetKey =
    [ "type", "subject", "origin", "year-accepted", "author.firstname", "pdf_copy" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


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
