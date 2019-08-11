module Data.Types exposing (Attribute, Document, DocumentId, DocumentResult, DocumentsPage, Filter(..), Filters, Folder, FolderCounts, FolderId, FolderType(..), FtsSorting(..), NodeType(..), SearchMethod(..), Selection, Window, WindowPage)

import Dict exposing (Dict)


type alias FolderId =
    ( Int, Float )



{- Actually, these types should be defined like this:

       type DocumentId = DocumentId Int

   But in Elm 0.19 union types are not comparable and therefore not usable as keys of a dict.
   Only ints, floats, chars, strings, lists, and tuples are comparable.
   So, as a workaround we use a tuple with some contrived structure to make it somewhat unique.
-}


type alias DocumentId =
    ( Float, Int )


type alias Folder =
    { id : FolderId
    , parent : Maybe FolderId
    , name : String
    , isCollection : Bool
    , numSubfolder : Int
    }


type NodeType
    = IsFolder FolderType
    | IsDocument
    | IsNeither


type FolderType
    = IsCollection
    | IsDirectory


type alias Selection =
    { scope : FolderId
    , searchMethod : SearchMethod
    , filters : Filters
    }


type SearchMethod
    = SelectByFolderListing
    | SelectByFullTextSearch String FtsSorting


type FtsSorting
    = ByRank
    | ByDate


type alias Filters =
    Dict String Filter


type Filter
    = YearWithin String String
    | TitleFts String


type alias FolderCounts =
    Dict FolderId Int


type alias Window =
    { offset : Int
    , limit : Int
    }


type alias DocumentsPage =
    WindowPage Document


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
    , attributes : List Attribute
    }


type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }
