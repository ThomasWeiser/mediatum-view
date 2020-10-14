module Tests.Types.Needs exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Types.Needs as Needs


type alias TheNeed =
    String


type alias TheNeeds =
    Needs.Needs TheNeed


needs0 : TheNeeds
needs0 =
    Needs.batch
        [ Needs.none
        , Needs.sequence Needs.none Needs.none
        , Needs.none
        , Needs.batch [ Needs.none, Needs.none, Needs.none ]
        , Needs.none
        ]


needs1 : TheNeeds
needs1 =
    Needs.batch
        [ Needs.none
        , Needs.sequence (Needs.atomic "1S1") (Needs.atomic "1S2")
        , Needs.none
        , Needs.atomic "2"
        , Needs.batch [ Needs.atomic "3B1", Needs.atomic "3B2" ]
        , Needs.none
        , Needs.atomic "4"
        , Needs.sequence (Needs.atomic "5S1") (Needs.atomic "5S2")
        , Needs.none
        , Needs.atomic "6"
        , Needs.batch [ Needs.atomic "7B1", Needs.atomic "7B2" ]
        , Needs.none
        , Needs.sequence (Needs.atomic "8") needs0
        , Needs.sequence needs0 (Needs.atomic "9")
        , Needs.none
        ]


needs1FlatList : List TheNeeds
needs1FlatList =
    [ Needs.sequence (Needs.atomic "1S1") (Needs.atomic "1S2")
    , Needs.atomic "2"
    , Needs.atomic "3B1"
    , Needs.atomic "3B2"
    , Needs.atomic "4"
    , Needs.sequence (Needs.atomic "5S1") (Needs.atomic "5S2")
    , Needs.atomic "6"
    , Needs.atomic "7B1"
    , Needs.atomic "7B2"
    , Needs.atomic "8"
    , Needs.atomic "9"
    ]


needs2 : TheNeeds
needs2 =
    Needs.batch
        [ Needs.none
        , needs1
        , Needs.none
        , Needs.atomic "---"
        , Needs.none
        , needs1
        , Needs.none
        ]


needs2FlatList : List TheNeeds
needs2FlatList =
    needs1FlatList ++ (Needs.atomic "---" :: needs1FlatList)


needs3 : TheNeeds
needs3 =
    Needs.sequence
        needs1
        (Needs.batch [ Needs.atomic "---", needs1 ])


needs3Flat : TheNeeds
needs3Flat =
    Needs.sequence
        (Needs.batch needs1FlatList)
        (Needs.batch (Needs.atomic "---" :: needs1FlatList))


suite : Test
suite =
    describe "Types.Needs"
        [ describe "flatten"
            [ describe "fixed test cases"
                [ test "needs0" <|
                    \() ->
                        Needs.flatten needs0
                            |> Expect.equal Needs.none
                , test "needs1" <|
                    \() ->
                        Needs.flatten needs1
                            |> Expect.equal (Needs.batch needs1FlatList)
                , test "needs2" <|
                    \() ->
                        Needs.flatten needs2
                            |> Expect.equal (Needs.batch needs2FlatList)
                , test "needs3" <|
                    \() ->
                        Needs.flatten needs3
                            |> Expect.equal needs3Flat
                ]
            ]
        ]
