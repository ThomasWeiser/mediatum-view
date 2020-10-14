module Entities.Folder exposing
    ( Folder
    , LineageFolders
    , dummy
    , init
    , isRoot
    , hasSubfolder
    )

{-| A folder is either a collection or a directory.

Folders are arranged in a hierarchy, with collections at the top and directories at the bottom.

@docs Folder
@docs LineageFolders
@docs dummy
@docs init
@docs isRoot
@docs hasSubfolder

-}

import List.Nonempty exposing (Nonempty)
import Types exposing (FolderDisplay(..))
import Types.Id as Id exposing (FolderId)


{-| -}
type alias Folder =
    { id : FolderId
    , parent : Maybe FolderId
    , name : String
    , display : FolderDisplay
    , numSubfolder : Int
    }


{-| A lineage of a folder denotes the path from this folder up to the root folder.

This type aggregates all `Folder` on that path.

-}
type alias LineageFolders =
    Nonempty Folder


{-| -}
dummy : Folder
dummy =
    { id = Id.fromInt -1
    , parent = Nothing
    , name = ""
    , display = DisplayAsCollection
    , numSubfolder = 0
    }


{-| -}
init : FolderId -> Maybe FolderId -> String -> FolderDisplay -> Int -> Folder
init id maybeParentId name display numSubfolder =
    { id = id
    , parent = maybeParentId
    , name = name
    , display = display
    , numSubfolder = numSubfolder
    }


{-| -}
isRoot : Folder -> Bool
isRoot folder =
    folder.parent == Nothing


{-| -}
hasSubfolder : Folder -> Bool
hasSubfolder folder =
    folder.numSubfolder > 0
