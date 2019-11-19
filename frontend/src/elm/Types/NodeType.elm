module Types.NodeType exposing (NodeType(..))

import Types.FolderDisplay exposing (FolderDisplay)


type NodeType
    = NodeIsFolder FolderDisplay
    | NodeIsDocument
    | NodeIsNeither
