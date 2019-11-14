module Types exposing
    ( Document
    , DocumentAttribute
    , DocumentResult
    , DocumentsPage
    , Filter(..)
    , Filters
    , FtsSorting(..)
    , NodeType(..)
    , SearchMethod(..)
    , Selection
    , Window
    , WindowPage
    )

import Dict
import Range exposing (Range)
import Sort.Dict
import Types.Folder as Folder exposing (Folder)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm exposing (SearchTerm)


type NodeType
    = NodeIsFolder Folder.Type
    | NodeIsDocument
    | NodeIsNeither


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
