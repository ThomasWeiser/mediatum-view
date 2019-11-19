module Entities.Folder exposing
    ( Folder
    , dummy
    , hasSubfolder
    , init
    , isRoot
    )

import Types.FolderDisplay exposing (FolderDisplay(..))
import Types.Id as Id exposing (FolderId)


type alias Folder =
    { id : FolderId
    , parent : Maybe FolderId
    , name : String
    , display : FolderDisplay
    , numSubfolder : Int
    }


dummy : Folder
dummy =
    { id = Id.fromInt -1
    , parent = Nothing
    , name = ""
    , display = DisplayAsCollection
    , numSubfolder = 0
    }


init : FolderId -> Maybe FolderId -> String -> FolderDisplay -> Int -> Folder
init id maybeParentId name display numSubfolder =
    { id = id
    , parent = maybeParentId
    , name = name
    , display = display
    , numSubfolder = numSubfolder
    }


isRoot : Folder -> Bool
isRoot folder =
    folder.parent == Nothing


hasSubfolder : Folder -> Bool
hasSubfolder folder =
    folder.numSubfolder > 0
