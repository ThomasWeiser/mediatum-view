module Types exposing
    ( FolderDisplay(..)
    , NodeType(..)
    , Window
    , WindowPage
    , orderingWindow
    )

import Ordering exposing (Ordering)


type FolderDisplay
    = DisplayAsCollection
    | DisplayAsDirectory


type NodeType
    = NodeIsFolder FolderDisplay
    | NodeIsDocument
    | NodeIsNeither


type alias Window =
    { offset : Int
    , limit : Int
    }


type alias WindowPage itemModel =
    { offset : Int
    , hasNextPage : Bool
    , content : List itemModel
    }


orderingWindow : Ordering Window
orderingWindow =
    Ordering.byField .offset
        |> Ordering.breakTiesWith
            (Ordering.byField .limit)
