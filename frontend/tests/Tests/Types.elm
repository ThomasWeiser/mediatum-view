module Tests.Types exposing
    ( fuzzerDocumentId
    , fuzzerDocumentIdFromSearch
    , fuzzerFacetFilters
    , fuzzerFolderId
    , fuzzerFtsFilters
    , fuzzerGlobalFts
    , fuzzerLimit
    , fuzzerMaskDocumentIdFromSearch
    , fuzzerMaskSelection
    , fuzzerNodeId
    , fuzzerOffset
    , fuzzerSelection
    , fuzzerSelectionFacets
    , fuzzerSelectionWindow
    , fuzzerSorting
    , fuzzerWindow
    , fuzzerYear
    )

import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Types.FilterList exposing (fuzzerFilterList)
import Tests.Types.SearchTerm exposing (fuzzerSearchTerm)
import Types exposing (DocumentIdFromSearch, Window)
import Types.Aspect as Aspect exposing (Aspect)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.Selection exposing (FacetFilters, FtsFilters, GlobalFts, Selection, Sorting(..))


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


fuzzerMaskName : Fuzzer String
fuzzerMaskName =
    [ "nodesmall_en", "nodesmall", "nodebig_en", "nodebig" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


fuzzerDocumentIdFromSearch : Fuzzer DocumentIdFromSearch
fuzzerDocumentIdFromSearch =
    Fuzz.map2 DocumentIdFromSearch
        (Fuzz.map Id.fromInt fuzzerId)
        (Fuzz.maybe fuzzerSearchTerm)


fuzzerMaskDocumentIdFromSearch : Fuzzer ( String, DocumentIdFromSearch )
fuzzerMaskDocumentIdFromSearch =
    Fuzz.map2 Tuple.pair
        fuzzerMaskName
        fuzzerDocumentIdFromSearch


fuzzerSelection : Fuzzer Selection
fuzzerSelection =
    Fuzz.map5 Selection
        fuzzerFolderId
        fuzzerGlobalFts
        fuzzerFtsFilters
        fuzzerFacetFilters
        fuzzerSorting


fuzzerSelectionWindow : Fuzzer ( Selection, Window )
fuzzerSelectionWindow =
    Fuzz.map2 Tuple.pair
        fuzzerSelection
        fuzzerWindow


fuzzerMaskSelection : Fuzzer ( String, Selection )
fuzzerMaskSelection =
    Fuzz.map2 Tuple.pair
        fuzzerMaskName
        fuzzerSelection


fuzzerSelectionFacets : Fuzzer ( Selection, List Aspect )
fuzzerSelectionFacets =
    Fuzz.map2 Tuple.pair
        fuzzerSelection
        (shortList 3 fuzzerFacet)


fuzzerFacet : Fuzzer Aspect
fuzzerFacet =
    Fuzz.oneOf
        [ [ "year"
          , "author"
          ]
            |> List.map Fuzz.constant
            |> Fuzz.oneOf
        , Fuzz.string
        ]
        |> Fuzz.map Aspect.fromString


fuzzerGlobalFts : Fuzzer GlobalFts
fuzzerGlobalFts =
    Fuzz.frequency
        [ ( 1, Fuzz.constant Nothing )
        , ( 1, Fuzz.map Just fuzzerSearchTerm )
        ]


fuzzerSorting : Fuzzer Sorting
fuzzerSorting =
    Fuzz.oneOf
        [ Fuzz.constant ByRank
        , Fuzz.constant ByDate
        ]


fuzzerFtsFilters : Fuzzer FtsFilters
fuzzerFtsFilters =
    fuzzerFilterList
        fuzzerSearchTerm


fuzzerFacetFilters : Fuzzer FacetFilters
fuzzerFacetFilters =
    fuzzerFilterList
        -- Do we want to test facet values with newlines or not?
        -- (Fuzz.string |> Fuzz.map (String.filter ((/=) '\n')))
        Fuzz.string


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
