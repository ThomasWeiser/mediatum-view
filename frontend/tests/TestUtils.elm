module TestUtils exposing
    ( just
    , justAndThen
    , justAndThenAll
    , nothing
    , shortList
    , testOrderingAntisymmetry
    , testOrderingProperties
    , testOrderingReflexivity
    , testOrderingTransitivity
    , testString
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List.Extra
import Test exposing (Test)


{-| Return a `Test` that evaluates a single Expectation on a string.
The same string is used as the name of the test.

Use this function if you don't want to write the same string twice (which could be error-prone).

-}
testString : String -> (String -> Expectation) -> Test
testString string thunk =
    Test.test string (\() -> thunk string)


{-| Passes if the `Maybe` is `Just x` and the value `x` passes the given test.
-}
justAndThen : (subject -> Expectation) -> Maybe subject -> Expectation
justAndThen thunk maybeSubject =
    case maybeSubject of
        Nothing ->
            Expect.fail "Nothing\n╷\n│ expect (Just ...)"

        Just value ->
            thunk value


{-| Passes if the `Maybe` is `Just x` and the value `x` passes the given list of tests.
-}
justAndThenAll : List (subject -> Expectation) -> Maybe subject -> Expectation
justAndThenAll thunks maybeSubject =
    case maybeSubject of
        Nothing ->
            Expect.fail "Nothing\n╷\n│ expect (Just ...)"

        Just value ->
            Expect.all thunks value


{-| Passes if the `Maybe` is a `Just` rather than `Nothing`.
This is useful for tests where you expect to get a `Just` but you don't care what the actual value is.
-}
just : Maybe subject -> Expectation
just maybeSubject =
    case maybeSubject of
        Nothing ->
            Expect.fail "Nothing\n╷\n│ expect (Just _)"

        Just value ->
            Expect.pass


{-| Passes if the `Maybe` is `Nothing` rather than a `Just`.
-}
nothing : Maybe subject -> Expectation
nothing maybeSubject =
    Expect.equal Nothing maybeSubject


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Probabilities are 0.5 for empty list, 0.25 for one element, 0.125 for two elements and so,
up to the given maximum length.
-}
shortList : Int -> Fuzzer a -> Fuzzer (List a)
shortList maxLength fuzzerElement =
    if maxLength <= 0 then
        Fuzz.constant []

    else
        Fuzz.oneOf
            [ Fuzz.constant []
            , Fuzz.map2
                (::)
                fuzzerElement
                (shortList (maxLength - 1) fuzzerElement)
            ]


{-| Test the reuqired properties of a strict total ordering on a given type:
reflexivity, antisymmetry and transitivity
-}
testOrderingProperties : String -> Fuzzer a -> (a -> a -> Order) -> Test
testOrderingProperties name fuzzer ordering =
    Test.describe name
        [ testOrderingReflexivity "reflexivity" fuzzer ordering
        , testOrderingAntisymmetry "antisymmetry" fuzzer ordering
        , testOrderingTransitivity "transitivity" fuzzer ordering
        ]


{-| Test the reflexivity of an ordering on a given type
-}
testOrderingReflexivity : String -> Fuzzer a -> (a -> a -> Order) -> Test
testOrderingReflexivity name fuzzer ordering =
    Test.fuzz fuzzer name <|
        \value ->
            ordering value value
                |> Expect.equal EQ


{-| Test the antisymmetry of an ordering on a given type
-}
testOrderingAntisymmetry : String -> Fuzzer a -> (a -> a -> Order) -> Test
testOrderingAntisymmetry name fuzzer ordering =
    Test.fuzz2 fuzzer fuzzer name <|
        \value1 value2 ->
            ordering value1 value2
                == EQ
                |> Expect.equal (value1 == value2)


{-| Test the transitivity of an ordering on a given type
-}
testOrderingTransitivity : String -> Fuzzer a -> (a -> a -> Order) -> Test
testOrderingTransitivity name fuzzer ordering =
    Test.fuzz3 fuzzer fuzzer fuzzer name <|
        \value1 value2 value3 ->
            let
                order12 =
                    ordering value1 value2

                order23 =
                    ordering value2 value3

                order13 =
                    ordering value1 value3
            in
            case ( order12, order23 ) of
                ( LT, LT ) ->
                    order13 |> Expect.equal LT

                ( LT, EQ ) ->
                    order13 |> Expect.equal LT

                ( LT, GT ) ->
                    Expect.pass

                ( EQ, LT ) ->
                    order13 |> Expect.equal LT

                ( EQ, EQ ) ->
                    order13 |> Expect.equal EQ

                ( EQ, GT ) ->
                    order13 |> Expect.equal GT

                ( GT, LT ) ->
                    Expect.pass

                ( GT, EQ ) ->
                    order13 |> Expect.equal GT

                ( GT, GT ) ->
                    order13 |> Expect.equal GT