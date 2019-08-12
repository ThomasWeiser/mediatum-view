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
            if .isCollection (List.Nonempty.head lineage) then
                Data.Types.NodeIsFolder Data.Types.FolderIsCollection

            else
                Data.Types.NodeIsFolder Data.Types.FolderIsDirectory

        IsDocument _ ->
            Data.Types.NodeIsDocument

        IsNeither ->
            Data.Types.NodeIsNeither
