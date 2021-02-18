module Entities.GenericNode exposing
    ( GenericNode(..)
    , toNodeType
    )

{-| An API request on the node data by an arbitrary node number results in either a document,
or a folder (together with all ancestor nodes), or neither.
The type `GenericNode` represents such a result.

@docs GenericNode
@docs toNodeType

-}

import Entities.Document exposing (Document)
import Entities.Folder exposing (LineageFolders)
import Entities.Residence exposing (Residence)
import List.Nonempty
import Types exposing (NodeType(..))


{-| -}
type GenericNode
    = IsFolder LineageFolders
    | IsDocument ( Document, Residence )
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
