module Folder
    exposing
        ( FolderId
        , Folder
        , init
        , idAsInt
        , view
        )

import Html exposing (Html)
import Html.Attributes


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
    , subfolderIds : Maybe (List FolderId)
    }


init : Int -> Int -> String -> Bool -> Bool -> Int -> Folder
init idAsInt parentIdAsInt name isToplevel isCollection numSubfolder =
    { id = ( idAsInt, 0.0 )
    , parent =
        if isToplevel then
            Nothing
        else
            Just ( parentIdAsInt, 0.0 )
    , name = name
    , isCollection = isCollection
    , numSubfolder = numSubfolder
    , subfolderIds = Nothing
    }


idAsInt : FolderId -> Int
idAsInt ( i, _ ) =
    i


hasSubfolder : Folder -> Bool
hasSubfolder folder =
    folder.numSubfolder > 0


view : Folder -> Bool -> Bool -> Html msg
view folder selected expanded =
    Html.div
        [ Html.Attributes.classList
            [ ( "folder-head", True )
            , ( "leaf", not (hasSubfolder folder) )
            , ( "expanded", hasSubfolder folder && expanded )
            , ( "collapsed", hasSubfolder folder && not expanded )
            , ( "selected", selected )
            ]
        ]
        [ Html.span
            [ Html.Attributes.class "folder-name" ]
            [ Html.text folder.name ]
        ]
