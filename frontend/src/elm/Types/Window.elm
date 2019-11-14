module Types.Window exposing
    ( Window
    , ordering
    )

import Ordering exposing (Ordering)


type alias Window =
    { offset : Int
    , limit : Int
    }


ordering : Ordering Window
ordering =
    Ordering.byField .offset
        |> Ordering.breakTiesWith
            (Ordering.byField .limit)
