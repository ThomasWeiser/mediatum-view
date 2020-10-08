module Entities.Residence exposing (Residence)

{-| The residnce of a document

@docs Residence, Lineage

-}

import List.Nonempty exposing (Nonempty)
import Types.Id exposing (FolderId)


{-| The residence of a document is the list of all folders (given as lineages) where it is located in the hierarchy.
-}
type alias Residence =
    List Lineage


{-| A lineage of a folder denotes the path from this folder up to the root folder.
Each folder on this path is given by its id.
-}
type alias Lineage =
    Nonempty FolderId
