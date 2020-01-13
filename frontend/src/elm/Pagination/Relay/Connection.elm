module Pagination.Relay.Connection exposing
    ( Connection
    , Edge
    , PageInfo
    , GraphqlObjects
    , nodes
    , connection
    , pageInfo
    )

{-|

@docs Connection
@docs Edge
@docs PageInfo
@docs GraphqlObjects

@docs nodes
@docs connection
@docs pageInfo

-}

import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)


{-| -}
type alias Connection cursorScalar nodeType =
    { pageInfo : PageInfo
    , edges : List (Edge cursorScalar nodeType)
    , totalCount : Int
    }


{-| -}
type alias Edge cursorScalar nodeType =
    { cursor : cursorScalar
    , node : nodeType
    }


{-| -}
type alias PageInfo =
    { hasNextPage : Bool
    , hasPreviousPage : Bool
    }


{-| -}
type alias GraphqlObjects graphqlObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType =
    { graphqlObjectsRecord
        | pageInfo :
            SelectionSet PageInfo pageInfoObject
            -> SelectionSet PageInfo connectionObject
        , totalCount : SelectionSet Int connectionObject
        , edges : SelectionSet (Edge cursorScalar nodeType) edgeObject -> SelectionSet (List (Edge cursorScalar nodeType)) connectionObject
        , cursor : SelectionSet (Maybe cursorScalar) edgeObject
        , node : SelectionSet nodeType nodeObject -> SelectionSet nodeType edgeObject
        , hasNextPage : SelectionSet Bool pageInfoObject
        , hasPreviousPage : SelectionSet Bool pageInfoObject
    }


{-| -}
nodes : Connection cursorModel nodeType -> List nodeType
nodes model =
    List.map .node model.edges


{-| -}
connection :
    GraphqlObjects graphqlObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType
    -> SelectionSet nodeType nodeObject
    -> SelectionSet (Connection cursorScalar nodeType) connectionObject
connection graphqlObjects nodeSelectionSet =
    SelectionSet.succeed Connection
        |> SelectionSet.with (graphqlObjects.pageInfo (pageInfo graphqlObjects))
        |> SelectionSet.with (graphqlObjects.edges (edge graphqlObjects nodeSelectionSet))
        |> SelectionSet.with graphqlObjects.totalCount


edge :
    GraphqlObjects graphqlObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType
    -> SelectionSet nodeType nodeObject
    -> SelectionSet (Edge cursorScalar nodeType) edgeObject
edge graphqlObjects nodeSelectionSet =
    SelectionSet.succeed Edge
        |> SelectionSet.with (graphqlObjects.cursor |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with (graphqlObjects.node nodeSelectionSet)


{-| -}
pageInfo :
    GraphqlObjects graphqlObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType
    -> SelectionSet PageInfo pageInfoObject
pageInfo graphqlObjects =
    SelectionSet.succeed PageInfo
        |> SelectionSet.with graphqlObjects.hasNextPage
        |> SelectionSet.with graphqlObjects.hasPreviousPage
