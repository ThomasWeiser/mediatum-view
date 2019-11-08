module GenericNode exposing (GenericNode(..), toNodeType)

import List.Nonempty exposing (Nonempty)
import Types exposing (Document, Folder)


type GenericNode
    = IsFolder (Nonempty Folder)
    | IsDocument Document
    | IsNeither


toNodeType : GenericNode -> Types.NodeType
toNodeType genericNode =
    case genericNode of
        IsFolder lineage ->
            Types.NodeIsFolder
                (.type_ (List.Nonempty.head lineage))

        IsDocument _ ->
            Types.NodeIsDocument

        IsNeither ->
            Types.NodeIsNeither
