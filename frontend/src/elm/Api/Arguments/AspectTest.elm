module Api.Arguments.AspectTest exposing (Test, Operation(..), testsAsGraphqlArgument)

{-| When using filters in a [`Selection`](Types-Selection) these filter
are translated into certain tests on aspects of the documents.

This module defines types to express these aspect tests
used to pass them as a parameter to the [query functions](Api-Queries).

Used internally in the `Api.*` modules.

@docs Test, Operation, testsAsGraphqlArgument

-}

import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Mediatum.Enum.AspectTestOperator as Operator
import Mediatum.InputObject exposing (AspectTestInput)


{-| -}
type alias Test =
    { name : String
    , operation : Operation
    }


{-| -}
type Operation
    = Equality String
    | Fts String


{-| -}
testsAsGraphqlArgument : List Test -> List (Maybe AspectTestInput)
testsAsGraphqlArgument tests =
    List.map
        (testInput >> Just)
        tests


testInput : Test -> AspectTestInput
testInput test =
    case test.operation of
        Equality value ->
            { name = Present test.name
            , operator = Present Operator.Equality
            , value = Present value
            }

        Fts value ->
            { name = Present test.name
            , operator = Present Operator.Fts
            , value = Present value
            }
