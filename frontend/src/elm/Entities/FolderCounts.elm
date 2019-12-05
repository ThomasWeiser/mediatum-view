module Entities.FolderCounts exposing
    ( FolderCounts
    , init
    , fromList
    )

{-|

@docs FolderCounts
@docs init
@docs fromList

-}

import Sort.Dict
import Types.Id as Id exposing (FolderId)
import Utils


{-| For a given search query the API can give the number of results in the selected folder and in all of its all subfolders.

This type represents such a distribution of counts of documents.

-}
type alias FolderCounts =
    Sort.Dict.Dict FolderId Int


{-| -}
init : FolderCounts
init =
    Sort.Dict.empty
        (Utils.sorter Id.ordering)


{-| -}
fromList : List ( FolderId, Int ) -> FolderCounts
fromList listOfPairs =
    Sort.Dict.fromList
        (Utils.sorter Id.ordering)
        listOfPairs
