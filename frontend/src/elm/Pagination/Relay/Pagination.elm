module Pagination.Relay.Pagination exposing
    ( Position(..)
    , paginationArguments
    )

{-|

@docs Position
@docs paginationArguments

-}

import Graphql.OptionalArgument exposing (OptionalArgument(..))
import List.Extra
import Pagination.Relay.Connection exposing (Connection)


{-| -}
type Position
    = First
    | Last
    | Next
    | Previous


type alias PaginationArguments optionals cursorModel =
    { optionals
        | first : OptionalArgument Int
        , last : OptionalArgument Int
        , before : OptionalArgument cursorModel
        , after : OptionalArgument cursorModel
    }


{-| -}
paginationArguments :
    Int
    -> Maybe (Connection cursorModel nodeType)
    -> Position
    -> PaginationArguments optionals cursorModel
    -> PaginationArguments optionals cursorModel
paginationArguments pageSize referencePage position =
    let
        anchor listSelector =
            case referencePage of
                Nothing ->
                    Absent

                Just { edges } ->
                    case
                        listSelector edges
                    of
                        Nothing ->
                            Absent

                        Just { cursor } ->
                            Present cursor
    in
    \optionals ->
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
                    , after = anchor List.Extra.last
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
                    , before = anchor List.head
                    , after = Absent
                }
