module Data.Types exposing
    ( Document
    , DocumentAttribute
    , DocumentId
    , DocumentResult
    , DocumentsPage
    , Filter(..)
    , Filters
    , Folder
    , FolderCounts
    , FolderId
    , FolderType(..)
    , FtsSorting(..)
    , NodeId
    , NodeType(..)
    , SearchMethod(..)
    , SearchTerm
    , Selection
    , Window
    , WindowPage
    , documentIdFromInt
    , documentIdToInt
    , folderIdFromInt
    , folderIdToInt
    , nodeIdFromInt
    , nodeIdToInt
    , searchTermFromString
    , searchTermFromStringWithDefault
    , searchTermToString
    )

import Dict
import Range exposing (Range)
import Sort.Dict
import String.Extra


type NodeId
    = NodeId Int


nodeIdToInt : NodeId -> Int
nodeIdToInt (NodeId id) =
    id


nodeIdFromInt : Int -> NodeId
nodeIdFromInt id =
    NodeId id


type FolderId
    = FolderId Int


folderIdToInt : FolderId -> Int
folderIdToInt (FolderId id) =
    id


folderIdFromInt : Int -> FolderId
folderIdFromInt id =
    FolderId id


type DocumentId
    = DocumentId Int


documentIdToInt : DocumentId -> Int
documentIdToInt (DocumentId id) =
    id


documentIdFromInt : Int -> DocumentId
documentIdFromInt id =
    DocumentId id


type alias Folder =
    { id : FolderId
    , parent : Maybe FolderId
    , name : String
    , type_ : FolderType
    , numSubfolder : Int
    }


type NodeType
    = NodeIsFolder FolderType
    | NodeIsDocument
    | NodeIsNeither


type FolderType
    = FolderIsCollection
    | FolderIsDirectory


type alias Selection =
    { scope : FolderId
    , searchMethod : SearchMethod
    , filters : Filters
    }


type SearchMethod
    = SelectByFolderListing
    | SelectByFullTextSearch SearchTerm FtsSorting


type FtsSorting
    = FtsByRank
    | FtsByDate


type alias Filters =
    Dict.Dict String Filter


type Filter
    = FilterYearWithin (Range Int)
    | FilterTitleFts SearchTerm


type alias FolderCounts =
    Sort.Dict.Dict FolderId Int


type alias Window =
    { offset : Int
    , limit : Int
    }


type alias DocumentsPage =
    WindowPage DocumentResult


type alias WindowPage itemModel =
    { offset : Int
    , hasNextPage : Bool
    , content : List itemModel
    }


type alias DocumentResult =
    { number : Int
    , distance : Float
    , document : Document
    }


type alias Document =
    { id : DocumentId
    , name : String
    , metadatatypeName : String
    , attributes : List DocumentAttribute
    }


type alias DocumentAttribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }


{-| A search term is a string that is

  - non-empty
  - has no whitespace at either end
  - has no repeated whitespace within

-}
type SearchTerm
    = SearchTerm String


searchTermFromString : String -> Maybe SearchTerm
searchTermFromString =
    String.Extra.clean
        >> String.Extra.nonEmpty
        >> Maybe.map SearchTerm


searchTermFromStringWithDefault : String -> String -> SearchTerm
searchTermFromStringWithDefault default string =
    searchTermFromString string
        |> Maybe.withDefault (SearchTerm default)


searchTermToString : SearchTerm -> String
searchTermToString (SearchTerm s) =
    s
