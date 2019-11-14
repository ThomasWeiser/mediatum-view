module Types.GenericNode exposing (GenericNode(..), toNodeType)

import List.Nonempty exposing (Nonempty)
import Types.Document exposing (Document)
import Types.Folder as Folder exposing (Folder)
import Types.NodeType exposing (NodeType(..))


type GenericNode
    = IsFolder (Nonempty Folder)
    | IsDocument Document
    | IsNeither


toNodeType : GenericNode -> NodeType
toNodeType genericNode =
    case genericNode of
        IsFolder lineage ->
            NodeIsFolder
                (.type_ (List.Nonempty.head lineage))

        IsDocument _ ->
            NodeIsDocument

        IsNeither ->
            NodeIsNeither
