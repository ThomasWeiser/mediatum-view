module Utils exposing
    ( findAdjacent
    , noBreakSpace
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


findAdjacent : List a -> (a -> Bool) -> Maybe ( Maybe a, a, Maybe a )
findAdjacent list predicate =
    let
        walk : a -> List a -> Maybe ( Maybe a, a, Maybe a )
        walk head1 tail1 =
            case tail1 of
                [] ->
                    Nothing

                head2 :: tail2 ->
                    if predicate head2 then
                        Just ( Just head1, head2, List.head tail2 )

                    else
                        walk head2 tail2
    in
    case list of
        [] ->
            Nothing

        head1 :: tail1 ->
            if predicate head1 then
                Just ( Nothing, head1, List.head tail1 )

            else
                walk head1 tail1
