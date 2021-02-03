module TestUtils exposing
    ( expectationList
    , fixpoint
    , just
    , justAndThen
    , justAndThenAll
    , nothing
    , shortList
    , shortListUniqueBy
    , testCoarseOrderingProperties
    , testFineOrderingProperties
    , testString
    )

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
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


{-| Passes if the value maps to itself.
-}
fixpoint : (a -> a) -> a -> Expectation
fixpoint mapping subject =
    Expect.equal subject (mapping subject)


{-| Passes if all expectations from a list pass. Otherwise fail on first failure.
-}
expectationList : List Expectation -> Expectation
expectationList expectations =
    Expect.all
        (Expect.pass
            {- Expect.all would fail on empty list -} :: expectations
            |> List.map always
        )
        ()


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


{-| A fuzzer for short lists with some uniqueness on the elements.

Works like `shortList`,
except that there will be no multiple elements that map to the same key value.

Note that the lists may get shorter than expected by dropping non-unique elements.

-}
shortListUniqueBy : (a -> comparable) -> Int -> Fuzzer a -> Fuzzer (List a)
shortListUniqueBy keyMapping maxLength fuzzerElement =
    shortList maxLength fuzzerElement
        |> Fuzz.map
            (\list ->
                list
                    |> List.map (\el -> ( keyMapping el, el ))
                    |> Dict.fromList
                    |> Dict.toList
                    |> List.map Tuple.second
            )


{-| Test the required properties of a strict total ordering on a given type.

"Fine": EQ must mean equality, i.e. not more than value per equivalence class.

-}
testFineOrderingProperties : String -> Fuzzer a -> (a -> a -> Order) -> Test
testFineOrderingProperties name fuzzer ordering =
    Test.describe name
        [ testOrderingReflexivity "reflexivity" fuzzer ordering
        , testOrderingSymmetry "symmetry" fuzzer ordering
        , testOrderingEquality "equality" fuzzer ordering
        , testOrderingTransitivity "transitivity" fuzzer ordering
        ]


{-| Test the required properties of a strict total ordering on a given type.

"Coarse": EQ may group more than one values together into one equivalence class.

-}
testCoarseOrderingProperties : String -> Fuzzer a -> (a -> a -> Order) -> Test
testCoarseOrderingProperties name fuzzer ordering =
    Test.describe name
        [ testOrderingReflexivity "reflexivity" fuzzer ordering
        , testOrderingSymmetry "symmetry" fuzzer ordering
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
testOrderingEquality : String -> Fuzzer a -> (a -> a -> Order) -> Test
testOrderingEquality name fuzzer ordering =
    Test.fuzz2 fuzzer fuzzer name <|
        \value1 value2 ->
            ordering value1 value2
                == EQ
                |> Expect.equal (value1 == value2)


{-| Test the transitivity of an ordering on a given type
-}
testOrderingSymmetry : String -> Fuzzer a -> (a -> a -> Order) -> Test
testOrderingSymmetry name fuzzer ordering =
    Test.fuzz2 fuzzer fuzzer name <|
        \value1 value2 ->
            let
                order12 =
                    ordering value1 value2

                order21 =
                    ordering value2 value1
            in
            case order12 of
                LT ->
                    order21 |> Expect.equal GT

                EQ ->
                    order21 |> Expect.equal EQ

                GT ->
                    order21 |> Expect.equal LT


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
