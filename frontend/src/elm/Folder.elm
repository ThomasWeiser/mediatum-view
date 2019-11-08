module Folder exposing
    ( dummy
    , hasSubfolder
    , init
    , isRoot
    )

import Types exposing (Folder, FolderId, FolderType)


dummy : Folder
dummy =
    { id = Types.folderIdFromInt -1
    , parent = Nothing
    , name = ""
    , type_ = Types.FolderIsCollection
    , numSubfolder = 0
    }


init : FolderId -> Maybe FolderId -> String -> FolderType -> Int -> Folder
init id maybeParentId name folderType numSubfolder =
    { id = id
    , parent = maybeParentId
    , name = name
    , type_ = folderType
    , numSubfolder = numSubfolder
    }


isRoot : Folder -> Bool
isRoot folder =
    folder.parent == Nothing


hasSubfolder : Folder -> Bool
hasSubfolder folder =
    folder.numSubfolder > 0
