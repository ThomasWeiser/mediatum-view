module Folder exposing
    ( Folder
    , FolderCounts
    , FolderId
    , dummy
    , hasSubfolder
    , idFromInt
    , idToInt
    , init
    , isRoot
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Icons


{-| Distinct type for folder identifier, which just wraps the integer that is used in the API
-}



{- Actually, the type should be defined like this:

       type FolderId = FolderId Int

   But in Elm 0.18 union types are not comparable and therefore not usable as keys of a dict.
   Only ints, floats, chars, strings, lists, and tuples are comparable.
   So, as a workaround we use a tuple with some contrived structure to make it somewhat unique.
-}


type alias FolderId =
    ( Int, Float )


type alias Folder =
    { id : FolderId
    , parent : Maybe FolderId
    , name : String
    , isCollection : Bool
    , numSubfolder : Int
    }


type alias FolderCounts =
    Dict FolderId Int


dummy : Folder
dummy =
    { id = idFromInt -1
    , parent = Nothing
    , name = ""
    , isCollection = False
    , numSubfolder = 0
    }


init : Int -> Maybe Int -> String -> Bool -> Int -> Folder
init idAsInt maybeParentIdAsInt name isCollection numSubfolder =
    { id = idFromInt idAsInt
    , parent =
        case maybeParentIdAsInt of
            Nothing ->
                Nothing

            Just parentIdAsInt ->
                Just (idFromInt parentIdAsInt)
    , name = name
    , isCollection = isCollection
    , numSubfolder = numSubfolder
    }


idToInt : FolderId -> Int
idToInt ( i, _ ) =
    i


idFromInt : Int -> FolderId
idFromInt idAsInt =
    ( idAsInt, 0.0 )


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
            , ( "collection", folder.isCollection )
            , ( "directory", not folder.isCollection )
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
