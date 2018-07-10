module Utils
    exposing
        ( noBreakSpace
        , when
        )

import Char


noBreakSpace : String
noBreakSpace =
    String.fromChar (Char.fromCode 160)


when : (a -> a) -> Bool -> a -> a
when fn cond =
    if cond then fn else identity
