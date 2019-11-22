module Entities.GenericNode exposing (GenericNode(..), toNodeType)

import Entities.Document exposing (Document)
import Entities.Folder exposing (Folder)
import List.Nonempty exposing (Nonempty)
import Types exposing (NodeType(..))


type GenericNode
    = IsFolder (Nonempty Folder)
    | IsDocument Document
    | IsNeither


toNodeType : GenericNode -> NodeType
toNodeType genericNode =
    case genericNode of
        IsFolder lineage ->
            NodeIsFolder
                (.display (List.Nonempty.head lineage))

        IsDocument _ ->
            NodeIsDocument

        IsNeither ->
            NodeIsNeither
