module Pagination
    exposing
        ( Page
        , Position(..)
        , ApiObjects
        , connectionPage
        , paginationArguments
        , totalCount
        , hasAdjacentPages
        , items
        )

import Graphqelm.Field
import Graphqelm.OptionalArgument exposing (OptionalArgument(Absent, Present))
import Graphqelm.SelectionSet exposing (SelectionSet, with)


type Page cursorModel itemModel
    = Page (Connection cursorModel itemModel)


type alias Connection cursorScalar itemModel =
    { totalCount : Int -- Not in relay spec! Added by PostGraphQL. See https://www.graphile.org/postgraphile/connections/
    , pageInfo : PageInfo cursorScalar
    , edges : List (Edge cursorScalar itemModel)
    }


type alias Edge cursorScalar itemModel =
    { cursor : cursorScalar
    , node : itemModel
    }


type alias PageInfo cursorScalar =
    { hasNextPage : Bool
    , hasPreviousPage : Bool
    , startCursor : Maybe cursorScalar -- Not in relay spec. Added by PostGraphQL
    , endCursor : Maybe cursorScalar -- Not in relay spec. Added by PostGraphQL
    }


type Position
    = First
    | Last
    | Next
    | Previous


totalCount : Page cursorModel itemModel -> Int
totalCount (Page model) =
    model.totalCount


items : Page cursorModel itemModel -> List itemModel
items (Page model) =
    List.map .node model.edges


hasAdjacentPages : Page cursorModel itemModel -> ( Bool, Bool )
hasAdjacentPages (Page model) =
    ( model.pageInfo.hasPreviousPage, model.pageInfo.hasNextPage )


type alias PaginationArguments optionals cursorModel =
    { optionals
        | first : OptionalArgument Int
        , last : OptionalArgument Int
        , before : OptionalArgument cursorModel
        , after : OptionalArgument cursorModel
    }


paginationArguments :
    Int
    -> Maybe (Page cursorModel itemModel)
    -> Position
    -> PaginationArguments optionals cursorModel
    -> PaginationArguments optionals cursorModel
paginationArguments pageSize referencePage position =
    let
        anchor selector =
            case referencePage of
                Nothing ->
                    Absent

                Just (Page page) ->
                    case
                        selector page.pageInfo
                    of
                        Nothing ->
                            Absent

                        Just cursor ->
                            Present cursor
    in
        (\optionals ->
            case position of
                First ->
                    { optionals
                        | first = Present pageSize
                        , last = Absent
                        , before = Absent
                        , after = Absent
                    }

                Next ->
                    { optionals
                        | first = Present pageSize
                        , last = Absent
                        , before = Absent
                        , after = anchor .endCursor
                    }

                Last ->
                    { optionals
                        | first = Absent
                        , last = Present pageSize
                        , before = Absent
                        , after = Absent
                    }

                Previous ->
                    { optionals
                        | first = Absent
                        , last = Present pageSize
                        , before = anchor .startCursor
                        , after = Absent
                    }
        )


type alias ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar itemModel =
    { apiObjectsRecord
        | connectionSelection :
            (Int -> PageInfo cursorScalar -> List (Edge cursorScalar itemModel) -> Connection cursorScalar itemModel)
            -> SelectionSet (Int -> PageInfo cursorScalar -> List (Edge cursorScalar itemModel) -> Connection cursorScalar itemModel) connectionObject
        , totalCount : Graphqelm.Field.Field (Maybe Int) connectionObject
        , pageInfo :
            SelectionSet (PageInfo cursorScalar) pageInfoObject
            -> Graphqelm.Field.Field (PageInfo cursorScalar) connectionObject
        , edges : SelectionSet (Edge cursorScalar itemModel) edgeObject -> Graphqelm.Field.Field (List (Edge cursorScalar itemModel)) connectionObject
        , edgeSelection :
            (cursorScalar -> itemModel -> Edge cursorScalar itemModel)
            -> SelectionSet (cursorScalar -> itemModel -> Edge cursorScalar itemModel) edgeObject
        , cursor : Graphqelm.Field.Field (Maybe cursorScalar) edgeObject
        , node : SelectionSet itemModel nodeObject -> Graphqelm.Field.Field itemModel edgeObject
        , pageInfoSelection :
            (Bool -> Bool -> Maybe cursorScalar -> Maybe cursorScalar -> PageInfo cursorScalar)
            -> SelectionSet (Bool -> Bool -> Maybe cursorScalar -> Maybe cursorScalar -> PageInfo cursorScalar) pageInfoObject
        , hasNextPage : Graphqelm.Field.Field Bool pageInfoObject
        , hasPreviousPage : Graphqelm.Field.Field Bool pageInfoObject
        , startCursor : Graphqelm.Field.Field (Maybe cursorScalar) pageInfoObject
        , endCursor : Graphqelm.Field.Field (Maybe cursorScalar) pageInfoObject
    }


{-| Extract items from paginated Relay connections
yielding a Page type that includes all additional pagination info
-}
connectionPage :
    ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar itemModel
    -> SelectionSet itemModel nodeObject
    -> SelectionSet (Page cursorScalar itemModel) connectionObject
connectionPage apiObjects nodeSelectionSet =
    connection apiObjects nodeSelectionSet
        |> Graphqelm.SelectionSet.map Page


connection :
    ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar itemModel
    -> SelectionSet itemModel nodeObject
    -> SelectionSet (Connection cursorScalar itemModel) connectionObject
connection apiObjects nodeSelectionSet =
    apiObjects.connectionSelection Connection
        |> with
            (apiObjects.totalCount
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with (apiObjects.pageInfo (pageInfo apiObjects))
        |> with
            (apiObjects.edges
                (apiObjects.edgeSelection Edge
                    |> with
                        (apiObjects.cursor
                            |> Graphqelm.Field.nonNullOrFail
                         -- |> Graphqelm.Field.map mapCursor
                        )
                    |> with
                        (apiObjects.node nodeSelectionSet)
                )
            )


pageInfo :
    ApiObjects apiObjectsRecord connectionObject edgeObject nodeObject pageInfoObject cursorScalar itemModel
    -> SelectionSet (PageInfo cursorScalar) pageInfoObject
pageInfo apiObjects =
    apiObjects.pageInfoSelection PageInfo
        |> with apiObjects.hasNextPage
        |> with apiObjects.hasPreviousPage
        |> with
            (apiObjects.startCursor
             -- |> Graphqelm.Field.map (Maybe.map mapCursor)
            )
        |> with
            (apiObjects.endCursor
             -- |> Graphqelm.Field.map (Maybe.map mapCursor)
            )
