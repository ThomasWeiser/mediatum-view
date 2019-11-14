module Types.NodeType exposing (NodeType(..))

import Types.Folder as Folder exposing (Folder)


type NodeType
    = NodeIsFolder Folder.Type
    | NodeIsDocument
    | NodeIsNeither
