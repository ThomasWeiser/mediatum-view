module Api.Arguments.AttributeTest exposing (Test, Operation(..), testsAsGraphqlArgument)

{-| When using filters in a [`Selection`](Types-Selection) these filter
are translated into certain tests on attributes of the documents.

This module defines types to express these attribute tests
used to pass them as a parameter to the [query functions](Api-Queries).

Used internally in the `Api.*` modules.

@docs Test, Operation, testsAsGraphqlArgument

-}

import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Mediatum.Enum.AttributeTestOperator as Operator
import Mediatum.InputObject exposing (AttributeTestInput)


{-| -}
type alias Test =
    { key : String
    , operation : Operation
    }


{-| -}
type Operation
    = Equality String
    | EqualityWithBlankNull String
    | ILike String (Maybe String)
    | SimpleFts String
    | DateRange ( String, String )


{-| -}
testsAsGraphqlArgument : List Test -> List (Maybe AttributeTestInput)
testsAsGraphqlArgument tests =
    List.map
        (testInput >> Just)
        tests


testInput : Test -> AttributeTestInput
testInput test =
    case test.operation of
        Equality value ->
            { key = Present test.key
            , operator = Present Operator.Equality
            , value = Present value
            , extra = Absent
            }

        EqualityWithBlankNull value ->
            { key = Present test.key
            , operator = Present Operator.Equalitywithblanknull
            , value = Present value
            , extra = Absent
            }

        ILike value extra ->
            { key = Present test.key
            , operator = Present Operator.Ilike
            , value = Present value
            , extra = Graphql.OptionalArgument.fromMaybe extra
            }

        SimpleFts value ->
            { key = Present test.key
            , operator = Present Operator.Simplefts
            , value = Present value
            , extra = Absent
            }

        DateRange ( fromDate, toDate ) ->
            { key = Present test.key
            , operator = Present Operator.Daterange
            , value = Present fromDate
            , extra = Present toDate
            }
