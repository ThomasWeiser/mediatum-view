module Types exposing
    ( FolderDisplay(..)
    , NodeType(..)
    , Window
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


orderingWindow : Ordering Window
orderingWindow =
    Ordering.byField .offset
        |> Ordering.breakTiesWith
            (Ordering.byField .limit)
