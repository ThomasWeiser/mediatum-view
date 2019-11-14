module Types.FolderCounts exposing
    ( FolderCounts
    , fromList
    , init
    )

import Sort.Dict
import Types.Id as Id exposing (FolderId)
import Utils


type alias FolderCounts =
    Sort.Dict.Dict FolderId Int


init : FolderCounts
init =
    Sort.Dict.empty
        (Utils.sorter Id.ordering)


fromList : List ( FolderId, Int ) -> FolderCounts
fromList listOfPairs =
    Sort.Dict.fromList
        (Utils.sorter Id.ordering)
        listOfPairs
