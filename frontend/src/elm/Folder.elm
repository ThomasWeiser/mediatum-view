module Folder exposing
    ( dummy
    , hasSubfolder
    , init
    , isRoot
    )

import Data.Types exposing (Folder, FolderCounts, FolderId, FolderType)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes


dummy : Folder
dummy =
    { id = Data.Types.folderIdFromInt -1
    , parent = Nothing
    , name = ""
    , type_ = Data.Types.FolderIsCollection
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
