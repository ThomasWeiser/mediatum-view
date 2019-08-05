module Query.Attribute exposing
    ( Operation(..)
    , Test
    , testsAsGraphqlArgument
    )

import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Mediatum.Enum.AttributeTestOperator as Operator
import Mediatum.InputObject exposing (AttributeTestInput)


type alias Test =
    { key : String
    , operation : Operation
    }


type Operation
    = Equality String
    | ILike String (Maybe String)
    | SimpleFts String
    | DateRange String String


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

        DateRange fromDate toDate ->
            { key = Present test.key
            , operator = Present Operator.Daterange
            , value = Present fromDate
            , extra = Present toDate
            }
