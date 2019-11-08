module Types.FolderId exposing
    ( FolderId
    , fromInt
    , ordering
    , toInt
    , toString
    )

import Ordering exposing (Ordering)


type FolderId
    = FolderId Int


toInt : FolderId -> Int
toInt (FolderId id) =
    id


fromInt : Int -> FolderId
fromInt id =
    FolderId id


toString : FolderId -> String
toString folderId =
    folderId |> toInt |> String.fromInt


ordering : Ordering FolderId
ordering (FolderId id1) (FolderId id2) =
    compare id1 id2
