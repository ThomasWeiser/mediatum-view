module Types.Ordering exposing
    ( orderingWindow
    , sorter
    )

import Dict
import Ordering exposing (..)
import Range
import Sort exposing (Sorter)
import Types exposing (..)
import Types.Id as Id exposing (FolderId)


sorter : Ordering a -> Sorter a
sorter ordering =
    Sort.custom ordering


orderingWindow : Ordering Window
orderingWindow =
    Ordering.byField .offset
        |> Ordering.breakTiesWith
            (Ordering.byField .limit)
