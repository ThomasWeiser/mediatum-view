module Types exposing
    ( DocumentResult
    , DocumentsPage
    , NodeType(..)
    , Window
    , WindowPage
    )

import Types.Document exposing (Document)
import Types.Folder as Folder exposing (Folder)
import Types.Id as Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm exposing (SearchTerm)


type NodeType
    = NodeIsFolder Folder.Type
    | NodeIsDocument
    | NodeIsNeither


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
