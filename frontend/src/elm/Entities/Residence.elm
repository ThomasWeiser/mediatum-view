module Entities.Residence exposing
    ( Residence, toList
    , Lineage
    )

{-| The residnce of a document

@docs Residence, toList

@docs Lineage

-}

import List.Nonempty exposing (Nonempty)
import Sort.Set
import Svg exposing (line)
import Types.Id as Id exposing (FolderId)
import Utils


{-| The residence of a document is the list of all folders (given as lineages) where it is located in the hierarchy.
-}
type alias Residence =
    List Lineage


{-| A lineage of a folder denotes the path from this folder up to the root folder.
Each folder on this path is given by its id.
-}
type alias Lineage =
    Nonempty FolderId


{-| Unique list of all folder ids in the residence.
-}
toList : Residence -> List FolderId
toList residence =
    List.foldl
        (\lineage set ->
            List.Nonempty.foldl
                Sort.Set.insert
                set
                lineage
        )
        (Sort.Set.empty (Utils.sorter Id.ordering))
        residence
        |> Sort.Set.toList
