module Types.Id exposing
    ( NodeId
    , FolderId
    , DocumentId
    , asNodeId
    , asFolderId
    , asDocumentId
    , toInt
    , fromInt
    , toString
    , ordering
    )

{-|

@docs NodeId
@docs FolderId
@docs DocumentId


#

@docs asNodeId
@docs asFolderId
@docs asDocumentId


#

@docs toInt
@docs fromInt
@docs toString


#

@docs ordering

-}

import Ordering exposing (Ordering)



{- We use a phantom type to represent distinguishable types of identifiers,
   that share a common implementation, i.e. common functions on it.
-}


type Id a
    = Id Int



{- Declare an emtpy dummy type for each id type.
   Will be used as the phantom type parameter.
-}


type Node
    = EmptyTypeConstructor_Node Node


type Folder
    = EmptyTypeConstructor_Folder Folder


type Document
    = EmptyTypeConstructor_Document Document



{- The instantiated phantom types are what we finally export.
   We declare a type alias for each one.
-}


{-| -}
type alias NodeId =
    Id Node


{-| -}
type alias FolderId =
    Id Folder


{-| -}
type alias DocumentId =
    Id Document


dummyValueToAvoidElmAnalyseErrors : ( Folder -> Folder, Node -> Node, Document -> Document )
dummyValueToAvoidElmAnalyseErrors =
    ( EmptyTypeConstructor_Folder
    , EmptyTypeConstructor_Node
    , EmptyTypeConstructor_Document
    )


{-| -}
asNodeId : Id a -> NodeId
asNodeId (Id i) =
    Id i


{-| -}
asFolderId : NodeId -> FolderId
asFolderId (Id i) =
    Id i


{-| -}
asDocumentId : NodeId -> DocumentId
asDocumentId (Id i) =
    Id i


{-| -}
toInt : Id a -> Int
toInt (Id id) =
    id


{-| -}
fromInt : Int -> Id a
fromInt id =
    Id id


{-| -}
toString : Id a -> String
toString nodeId =
    nodeId |> toInt |> String.fromInt


{-| -}
ordering : Ordering (Id a)
ordering (Id id1) (Id id2) =
    compare id1 id2
