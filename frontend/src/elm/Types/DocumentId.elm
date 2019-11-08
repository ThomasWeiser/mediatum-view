module Types.DocumentId exposing
    ( DocumentId
    , fromInt
    , ordering
    , toInt
    , toString
    )

import Ordering exposing (Ordering)


type DocumentId
    = DocumentId Int


toInt : DocumentId -> Int
toInt (DocumentId id) =
    id


fromInt : Int -> DocumentId
fromInt id =
    DocumentId id


toString : DocumentId -> String
toString documentId =
    documentId |> toInt |> String.fromInt


ordering : Ordering DocumentId
ordering (DocumentId id1) (DocumentId id2) =
    compare id1 id2
