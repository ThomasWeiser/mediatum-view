module Utils exposing
    ( noBreakSpace
    , tupleAddThird
    , tupleRemoveThird
    , when
    )

import Char


noBreakSpace : String
noBreakSpace =
    String.fromChar (Char.fromCode 160)


when : (a -> a) -> Bool -> a -> a
when fn cond =
    if cond then
        fn

    else
        identity


tupleAddThird : c -> ( a, b ) -> ( a, b, c )
tupleAddThird c ( a, b ) =
    ( a, b, c )


tupleRemoveThird : ( a, b, c ) -> ( a, b )
tupleRemoveThird ( a, b, c ) =
    ( a, b )
