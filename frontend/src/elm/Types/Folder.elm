module Types.Folder exposing
    ( Folder
    , Type(..)
    , dummy
    , hasSubfolder
    , init
    , isRoot
    )

import Types.FolderId as FolderId exposing (FolderId)


type alias Folder =
    { id : FolderId
    , parent : Maybe FolderId
    , name : String
    , type_ : Type
    , numSubfolder : Int
    }


type Type
    = IsCollection
    | IsDirectory


dummy : Folder
dummy =
    { id = FolderId.fromInt -1
    , parent = Nothing
    , name = ""
    , type_ = IsCollection
    , numSubfolder = 0
    }


init : FolderId -> Maybe FolderId -> String -> Type -> Int -> Folder
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
