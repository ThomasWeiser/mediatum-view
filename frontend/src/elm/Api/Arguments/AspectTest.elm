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
import Types.Aspect as Aspect exposing (Aspect)


{-| -}
type alias Test =
    { aspect : Aspect
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
            { name = Present <| Aspect.toString test.aspect
            , operator = Present Operator.Equality
            , value = Present value
            }

        Fts value ->
            { name = Present <| Aspect.toString test.aspect
            , operator = Present Operator.Fts
            , value = Present value
            }
