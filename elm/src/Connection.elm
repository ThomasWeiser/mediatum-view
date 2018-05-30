module Connection
    exposing
        ( Connection
        , Edge
        , PageInfo
        , ApiObjects
        , connection
        , pageInfo
        , nodes
        )

import Graphqelm.Field
import Graphqelm.SelectionSet exposing (SelectionSet, with)


type alias Connection cursorScalar nodeType =
    { pageInfo : PageInfo
    , edges : List (Edge cursorScalar nodeType)
    , totalCount : Int
    }


type alias Edge cursorScalar nodeType =
    { cursor : cursorScalar
    , node : nodeType
    }


type alias PageInfo =
    { hasNextPage : Bool
    , hasPreviousPage : Bool
    }


type alias ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType =
    { apiObjectsRecord
        | connectionSelection :
            (PageInfo -> List (Edge cursorScalar nodeType) -> Int -> Connection cursorScalar nodeType)
            -> SelectionSet (PageInfo -> List (Edge cursorScalar nodeType) -> Int -> Connection cursorScalar nodeType) connectionObject
        , pageInfo :
            SelectionSet PageInfo pageInfoObject
            -> Graphqelm.Field.Field PageInfo connectionObject
        , totalCount : Graphqelm.Field.Field (Maybe Int) connectionObject
        , edges : SelectionSet (Edge cursorScalar nodeType) edgeObject -> Graphqelm.Field.Field (List (Edge cursorScalar nodeType)) connectionObject
        , edgeSelection :
            (cursorScalar -> nodeType -> Edge cursorScalar nodeType)
            -> SelectionSet (cursorScalar -> nodeType -> Edge cursorScalar nodeType) edgeObject
        , cursor : Graphqelm.Field.Field (Maybe cursorScalar) edgeObject
        , node : SelectionSet nodeType nodeObject -> Graphqelm.Field.Field nodeType edgeObject
        , pageInfoSelection :
            (Bool -> Bool -> PageInfo)
            -> SelectionSet (Bool -> Bool -> PageInfo) pageInfoObject
        , hasNextPage : Graphqelm.Field.Field Bool pageInfoObject
        , hasPreviousPage : Graphqelm.Field.Field Bool pageInfoObject
    }


nodes : Connection cursorModel nodeType -> List nodeType
nodes model =
    List.map .node model.edges


connection :
    ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType
    -> SelectionSet nodeType nodeObject
    -> SelectionSet (Connection cursorScalar nodeType) connectionObject
connection apiObjects nodeSelectionSet =
    apiObjects.connectionSelection Connection
        |> with (apiObjects.pageInfo (pageInfo apiObjects))
        |> with (apiObjects.edges (edge apiObjects nodeSelectionSet))
        |> with (apiObjects.totalCount |> Graphqelm.Field.nonNullOrFail)


edge :
    ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType
    -> SelectionSet nodeType nodeObject
    -> SelectionSet (Edge cursorScalar nodeType) edgeObject
edge apiObjects nodeSelectionSet =
    apiObjects.edgeSelection Edge
        |> with (apiObjects.cursor |> Graphqelm.Field.nonNullOrFail)
        |> with (apiObjects.node nodeSelectionSet)


pageInfo :
    ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar nodeType
    -> SelectionSet PageInfo pageInfoObject
pageInfo apiObjects =
    apiObjects.pageInfoSelection PageInfo
        |> with apiObjects.hasNextPage
        |> with apiObjects.hasPreviousPage
