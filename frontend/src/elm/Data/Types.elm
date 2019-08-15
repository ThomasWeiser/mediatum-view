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

import Dict exposing (Dict)


type alias NodeId =
    ( Int, Float, Float )


nodeIdToInt : NodeId -> Int
nodeIdToInt ( id, _, _ ) =
    id


nodeIdFromInt : Int -> NodeId
nodeIdFromInt id =
    ( id, 0.0, 0.0 )


type alias FolderId =
    ( Int, Float )


folderIdToInt : FolderId -> Int
folderIdToInt ( id, _ ) =
    id


folderIdFromInt : Int -> FolderId
folderIdFromInt id =
    ( id, 0.0 )



{- Actually, these types should be defined like this:

       type DocumentId = DocumentId Int

   But in Elm 0.19 union types are not comparable and therefore not usable as keys of a dict.
   Only ints, floats, chars, strings, lists, and tuples are comparable.
   So, as a workaround we use a tuple with some contrived structure to make it somewhat unique.

   TODO: Can now be done if we consequently use Sort.Dict
   TODO: GenericId
-}


type alias DocumentId =
    ( Float, Int )


documentIdToInt : DocumentId -> Int
documentIdToInt ( _, id ) =
    id


documentIdFromInt : Int -> DocumentId
documentIdFromInt id =
    ( 0.0, id )


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
    Dict String Filter


type Filter
    = FilterYearWithin String String
    | FilterTitleFts String


type alias FolderCounts =
    Dict FolderId Int


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
