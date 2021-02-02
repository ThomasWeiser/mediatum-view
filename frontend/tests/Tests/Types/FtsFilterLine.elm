module Tests.Types.FtsFilterLine exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TestUtils exposing (..)
import Types.Aspect as Aspect exposing (Aspect)
import Types.FtsFilterLine exposing (..)


aspect =
    { a = Aspect.fromString "A"
    , b = Aspect.fromString "B"
    , c = Aspect.fromString "C"
    , d = Aspect.fromString "D"
    , e = Aspect.fromString "E"
    , f = Aspect.fromString "F"
    , z = Aspect.fromString "Z"
    }


m1 : FtsFilterLines
m1 =
    [ ( aspect.z, "" )
    , ( aspect.a, "A" )
    , ( aspect.b, "" )
    , ( aspect.c, "C" )
    , ( aspect.d, "" )
    , ( aspect.e, "E" )
    ]


suite : Test
suite =
    describe "Types.FtsFilterLine"
        [ test "Route (a c)" <|
            \() ->
                mergeFtsFilterLines
                    [ ( aspect.a, "An" )
                    , ( aspect.c, "Cn" )
                    ]
                    m1
                    |> Expect.equal
                        [ ( aspect.z, "" )
                        , ( aspect.a, "An" )
                        , ( aspect.b, "" )
                        , ( aspect.c, "Cn" )
                        , ( aspect.d, "" )
                        ]
        , test "Route (c a)" <|
            \() ->
                mergeFtsFilterLines
                    [ ( aspect.c, "Cn" )
                    , ( aspect.a, "An" )
                    ]
                    m1
                    |> Expect.equal
                        [ ( aspect.z, "" )
                        , ( aspect.c, "Cn" )
                        , ( aspect.a, "An" )
                        , ( aspect.b, "" )
                        , ( aspect.d, "" )
                        ]
        , test "Route (b)" <|
            \() ->
                mergeFtsFilterLines
                    [ ( aspect.b, "Bn" )
                    ]
                    m1
                    |> Expect.equal
                        [ ( aspect.z, "" )
                        , ( aspect.b, "Bn" )
                        , ( aspect.d, "" )
                        ]
        , test "Route (b d)" <|
            \() ->
                mergeFtsFilterLines
                    [ ( aspect.b, "Bn" )
                    , ( aspect.d, "Dn" )
                    ]
                    m1
                    |> Expect.equal
                        [ ( aspect.z, "" )
                        , ( aspect.b, "Bn" )
                        , ( aspect.d, "" )
                        ]
        , test "Route (d b)" <|
            \() ->
                mergeFtsFilterLines
                    [ ( aspect.d, "Dn" )
                    , ( aspect.b, "Bn" )
                    ]
                    m1
                    |> Expect.equal
                        [ ( aspect.z, "" )
                        , ( aspect.b, "Bn" )
                        , ( aspect.d, "" )
                        ]
        ]
