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
    , Selection
    , Window
    , WindowPage
    , documentIdFromInt
    , documentIdToInt
    , folderIdFromInt
    , folderIdToInt
    , nodeIdFromInt
    , nodeIdToInt
    )

import Dict
import Sort.Dict


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
    , isCollection : Bool
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
    | SelectByFullTextSearch String FtsSorting


type FtsSorting
    = FtsByRank
    | FtsByDate


type alias Filters =
    Dict.Dict String Filter


type Filter
    = FilterYearWithin String String
    | FilterTitleFts String


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
