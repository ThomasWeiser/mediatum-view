module Types.Ordering exposing (sorter)

import Ordering exposing (..)
import Sort exposing (Sorter)


sorter : Ordering a -> Sorter a
sorter ordering =
    Sort.custom ordering
