module Folder
    exposing
        ( FolderId
        , Folder
        , init
        , idAsInt
        , toggle
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
    , name : String
    , isCollection : Bool
    , numSubfolder : Int
    , isExpanded : Bool
    , subfolderIds : Maybe (List FolderId)
    }


init : Int -> String -> Bool -> Int -> Folder
init idAsInt name isCollection numSubfolder =
    { id = ( idAsInt, 0.0 )
    , name = name
    , isCollection = isCollection
    , numSubfolder = numSubfolder
    , isExpanded = False
    , subfolderIds = Nothing
    }


idAsInt : FolderId -> Int
idAsInt ( i, _ ) =
    i


toggle : Folder -> Folder
toggle folder =
    { folder | isExpanded = not folder.isExpanded }


hasSubfolder : Folder -> Bool
hasSubfolder folder =
    folder.numSubfolder > 0


view : Folder -> Html msg
view folder =
    Html.div
        [ Html.Attributes.classList
            [ ( "leaf", not (hasSubfolder folder) )
            , ( "expanded", hasSubfolder folder && folder.isExpanded )
            , ( "collapsed", hasSubfolder folder && not (folder.isExpanded) )
            , ( "folder-head", True )
            ]
        ]
        [ Html.span
            [ Html.Attributes.class "folder-name" ]
            [ Html.text folder.name ]
        ]
