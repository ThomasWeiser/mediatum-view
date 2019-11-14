module Types.FolderCounts exposing
    ( FolderCounts
    , fromList
    , init
    )

import Sort.Dict
import Types.Id as Id exposing (FolderId)
import Types.Ordering


type alias FolderCounts =
    Sort.Dict.Dict FolderId Int


init : FolderCounts
init =
    Sort.Dict.empty
        (Types.Ordering.sorter Id.ordering)


fromList : List ( FolderId, Int ) -> FolderCounts
fromList listOfPairs =
    Sort.Dict.fromList
        (Types.Ordering.sorter Id.ordering)
        listOfPairs
