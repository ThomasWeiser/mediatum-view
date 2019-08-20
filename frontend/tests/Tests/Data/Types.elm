module Tests.Data.Types exposing
    ( fuzzerDocumentId
    , fuzzerFilter
    , fuzzerFilters
    , fuzzerFolderId
    , fuzzerFtsSorting
    , fuzzerNodeId
    , fuzzerSearchMethod
    , fuzzerSelection
    , fuzzerSelectionWindow
    , fuzzerWindow
    )

import Basics.Extra
import Data.Types exposing (..)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import String.Extra
import Test exposing (..)
import TestUtils exposing (..)
import Tests.Range


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
    Fuzz.map folderIdFromInt fuzzerId


fuzzerNodeId : Fuzzer NodeId
fuzzerNodeId =
    Fuzz.map nodeIdFromInt fuzzerId


fuzzerDocumentId : Fuzzer DocumentId
fuzzerDocumentId =
    Fuzz.map documentIdFromInt fuzzerId


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
        (Fuzz.oneOf
            [ Fuzz.constant 0
            , fuzzerOffsetOrLimit
            ]
        )
        fuzzerOffsetOrLimit


fuzzerOffsetOrLimit : Fuzzer Int
fuzzerOffsetOrLimit =
    Fuzz.frequency
        [ ( 5, Fuzz.constant 5 )
        , ( 50, Fuzz.constant 10 )
        , ( 10, Fuzz.constant 20 )
        , ( 5, Fuzz.constant 50 )
        , ( 30, Fuzz.intRange 0 200 )
        ]


{-| A fuzzer for canonical search terms,
i.e. without whitespace at the left and the right
and without repeated whitespace within the string.

It generates some common search string formats as well as random search strings.

-}
fuzzerSearchTerm : Fuzzer String
fuzzerSearchTerm =
    Fuzz.oneOf
        [ [ "foo"
          , "foo bar"
          , "\"foo\""
          , "\"foo bar\""
          , "\"foo bar\" baz"
          , "-foo"
          , "foo -bar -baz"
          , "-\"foo bar\" baz"
          ]
            |> List.map Fuzz.constant
            |> Fuzz.oneOf
        , Fuzz.string
            |> Fuzz.map
                (String.Extra.clean >> String.Extra.nonBlank >> Maybe.withDefault "baz")
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
