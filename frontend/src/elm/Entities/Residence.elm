module Entities.Residence exposing (Residence, limitToToplevelFolders, toList)

{-| The residence of a document, i.e. the set of folders in which the document appears.

@docs Residence, limitToToplevelFolders, toList

-}

import List.Nonempty
import Sort.Set
import Types.Config exposing (Config)
import Types.Id as Id exposing (FolderId, LineageIds)
import Utils


{-| The residence of a document is the list of all folders (given as lineages)
where it is located in the hierarchy.
-}
type alias Residence =
    List LineageIds


{-| Limit the lineages in relation to a list of toplevel folders.

Drop those lineages that are not rooted in one of the toplevel folders.

-}
limitToToplevelFolders : Config -> Residence -> Residence
limitToToplevelFolders config =
    List.filterMap
        (Id.limitToToplevelFolders config.toplevelFolderIds)


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
