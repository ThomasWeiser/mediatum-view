module Folder exposing
    ( dummy
    , hasSubfolder
    , init
    , isRoot
    , view
    )

import Data.Types exposing (Folder, FolderCounts, FolderId, FolderType)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Icons


{-| Distinct type for folder identifier, which just wraps the integer that is used in the API
-}



{- Actually, the type should be defined like this:

       type FolderId = FolderId Int

   But in Elm 0.19 union types are not comparable and therefore not usable as keys of a dict.
   Only ints, floats, chars, strings, lists, and tuples are comparable.
   So, as a workaround we use a tuple with some contrived structure to make it somewhat unique.
-}


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


view : Folder -> Maybe Int -> Bool -> Bool -> Html msg
view folder maybeCount selected expanded =
    Html.div
        [ Html.Attributes.classList
            [ ( "folder-head", True )
            , ( "collection", folder.type_ == Data.Types.FolderIsCollection )
            , ( "directory", folder.type_ == Data.Types.FolderIsDirectory )
            , ( "collapsed", hasSubfolder folder && not expanded )
            , ( "expanded", hasSubfolder folder && expanded )
            , ( "leaf", not (hasSubfolder folder) )
            , ( "selected", selected )
            ]
        ]
        ([ if hasSubfolder folder then
            Icons.expando

           else
            Icons.leaf
         , Html.span
            [ Html.Attributes.class "folder-name" ]
            [ Html.text folder.name ]
         ]
            ++ (case maybeCount of
                    Nothing ->
                        []

                    Just count ->
                        [ Html.text " "
                        , Html.span
                            [ Html.Attributes.class "folder-count" ]
                            [ Html.text <| "(" ++ String.fromInt count ++ ")" ]
                        ]
               )
        )
