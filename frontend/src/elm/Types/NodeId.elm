module Types.NodeId exposing
    ( NodeId
    , fromInt
    , ordering
    , toInt
    , toString
    )

import Ordering exposing (Ordering)


type NodeId
    = NodeId Int


toInt : NodeId -> Int
toInt (NodeId id) =
    id


fromInt : Int -> NodeId
fromInt id =
    NodeId id


toString : NodeId -> String
toString nodeId =
    nodeId |> toInt |> String.fromInt


ordering : Ordering NodeId
ordering (NodeId id1) (NodeId id2) =
    compare id1 id2
