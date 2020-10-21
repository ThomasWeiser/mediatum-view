module Entities.Residence exposing (Residence, toList)

{-| The residence of a document, i.e. the set of folders in which the document appears.

@docs Residence, toList

-}

import List.Nonempty exposing (Nonempty)
import Sort.Set
import Types.Id as Id exposing (FolderId, LineageIds)
import Utils


{-| The residence of a document is the list of all folders (given as lineages)
where it is located in the hierarchy.
-}
type alias Residence =
    List LineageIds


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
