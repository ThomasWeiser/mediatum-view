module Types.NodeType exposing (NodeType(..))

import Entities.Folder as Folder exposing (Folder)


type NodeType
    = NodeIsFolder Folder.Type
    | NodeIsDocument
    | NodeIsNeither
