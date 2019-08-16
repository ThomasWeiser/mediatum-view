module Utils exposing
    ( findAdjacent
    , ifElse
    , lexicalOrder
    , noBreakSpace
    , onChange
    , tupleAddThird
    , tupleRemoveThird
    , when
    )

import Char
import Html
import Html.Events
import Json.Decode


noBreakSpace : String
noBreakSpace =
    String.fromChar (Char.fromCode 160)


{-| Return the first argument if the given boolean is `True`. Otherwise, return the second argument.
-}
ifElse : a -> a -> Bool -> a
ifElse ifTrue ifFalse bool =
    if bool then
        ifTrue

    else
        ifFalse


when : (a -> a) -> Bool -> a -> a
when fn =
    ifElse fn identity


tupleAddThird : c -> ( a, b ) -> ( a, b, c )
tupleAddThird c ( a, b ) =
    ( a, b, c )


tupleRemoveThird : ( a, b, c ) -> ( a, b )
tupleRemoveThird ( a, b, c ) =
    ( a, b )


findAdjacent : (a -> Bool) -> List a -> Maybe ( Maybe a, a, Maybe a )
findAdjacent predicate list =
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


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.on "change"
        (Json.Decode.map tagger Html.Events.targetValue)



-- TODO: Suggest for elm-community/list-extra, and posssibly also for matthewsj/elm-ordering


lexicalOrder : (a -> a -> Order) -> List a -> List a -> Order
lexicalOrder compareElements listL listR =
    case ( listL, listR ) of
        ( [], [] ) ->
            EQ

        ( [], _ :: _ ) ->
            LT

        ( _ :: _, [] ) ->
            GT

        ( headL :: tailL, headR :: tailR ) ->
            case compareElements headL headR of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    lexicalOrder compareElements tailL tailR
