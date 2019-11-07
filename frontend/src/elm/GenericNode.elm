module GenericNode exposing (GenericNode(..), toNodeType)

import Data.Types exposing (Document, Folder)
import List.Nonempty exposing (Nonempty)


type GenericNode
    = IsFolder (Nonempty Folder)
    | IsDocument Document
    | IsNeither


toNodeType : GenericNode -> Data.Types.NodeType
toNodeType genericNode =
    case genericNode of
        IsFolder lineage ->
            Data.Types.NodeIsFolder
                (.type_ (List.Nonempty.head lineage))

        IsDocument _ ->
            Data.Types.NodeIsDocument

        IsNeither ->
            Data.Types.NodeIsNeither
