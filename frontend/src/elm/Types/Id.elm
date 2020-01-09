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


# Types for node identifiers

@docs NodeId
@docs FolderId
@docs DocumentId


# Converting from one node id type into another

@docs asNodeId
@docs asFolderId
@docs asDocumentId


# Converting to/from unwrapped types

@docs toInt
@docs fromInt
@docs toString


# Miscellaneous

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


{-| A number used as a node identifier.

We don't know or don't specify whether this node represents a folder, a document or some other node type.

-}
type alias NodeId =
    Id Node


{-| A number used as a folder identifier.
-}
type alias FolderId =
    Id Folder


{-| A number used as a document identifier.
-}
type alias DocumentId =
    Id Document


dummyValueToAvoidElmAnalyseErrors : ( Folder -> Folder, Node -> Node, Document -> Document )
dummyValueToAvoidElmAnalyseErrors =
    ( EmptyTypeConstructor_Folder
    , EmptyTypeConstructor_Node
    , EmptyTypeConstructor_Document
    )


{-| Convert any id type into a `NodeId`.
-}
asNodeId : Id a -> NodeId
asNodeId (Id i) =
    Id i


{-| Convert a `NodeId` into a `FolderId`. Only use this if you know that this node is a folder.
-}
asFolderId : NodeId -> FolderId
asFolderId (Id i) =
    Id i


{-| Convert a `NodeId` into a `DocumentId`. Only use this if you know that this node is a document.
-}
asDocumentId : NodeId -> DocumentId
asDocumentId (Id i) =
    Id i


{-| Get the node number
-}
toInt : Id a -> Int
toInt (Id id) =
    id


{-| Construct an id from a number
-}
fromInt : Int -> Id a
fromInt id =
    Id id


{-| Get the node number as a string
-}
toString : Id a -> String
toString nodeId =
    nodeId |> toInt |> String.fromInt


{-| Define an ordering on the type so we can use it as a key in a `Sort.Dict`.
-}
ordering : Ordering (Id a)
ordering (Id id1) (Id id2) =
    compare id1 id2
