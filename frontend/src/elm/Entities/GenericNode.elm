module Entities.GenericNode exposing
    ( GenericNode(..)
    , toNodeType
    )

{-| An API request by an arbitrary node number results in either a document, or a folder (together with all ancestor nodes), or neither. The type `GenericNode` represents such a result.

@docs GenericNode
@docs toNodeType

-}

import Entities.Document exposing (Document)
import Entities.Folder exposing (Folder)
import List.Nonempty exposing (Nonempty)
import Types exposing (NodeType(..))


{-| -}
type GenericNode
    = IsFolder (Nonempty Folder)
    | IsDocument Document
    | IsNeither


{-| -}
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
