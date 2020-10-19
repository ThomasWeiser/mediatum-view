module Utils exposing
    ( ifElse
    , when
    , tupleAddThird
    , tupleRemoveThird
    , prependIf
    , findMap
    , findAdjacent
    , lexicalOrdering
    , mapWhile
    , mapEllipsis
    , remoteDataCheck
    , remoteDataMapFallible
    , sorter
    , noBreakSpace
    , onChange
    , maybeOrdering
    )

{-|


# Bool

@docs ifElse
@docs when


# Tuple

@docs tupleAddThird
@docs tupleRemoveThird


# List

@docs prependIf
@docs findMap
@docs findAdjacent
@docs lexicalOrdering
@docs mapWhile
@docs mapEllipsis


# RemoteData

@docs remoteDataCheck
@docs remoteDataMapFallible


# Ordering

@docs sorter


# Html

@docs noBreakSpace
@docs onChange

-}

import Char
import Html
import Html.Events
import Json.Decode
import Ordering exposing (Ordering)
import RemoteData exposing (RemoteData)
import Sort exposing (Sorter)


{-| Return the first argument if the given boolean is `True`. Otherwise, return the second argument.
-}
ifElse : a -> a -> Bool -> a
ifElse ifTrue ifFalse bool =
    if bool then
        ifTrue

    else
        ifFalse


{-| Conditionally apply a function
-}
when : (a -> a) -> Bool -> a -> a
when fn =
    ifElse fn identity


{-| -}
tupleAddThird : c -> ( a, b ) -> ( a, b, c )
tupleAddThird c ( a, b ) =
    ( a, b, c )


{-| -}
tupleRemoveThird : ( a, b, c ) -> ( a, b )
tupleRemoveThird ( a, b, _ ) =
    ( a, b )


{-| Conditionally add an element to the front of a list.
-}
prependIf : a -> Bool -> List a -> List a
prependIf element condition list =
    if condition then
        element :: list

    else
        list


{-| Find the first element that maps to a `Just b` and return this mapping.
If none match, return Nothing.
-}
findMap : (a -> Maybe b) -> List a -> Maybe b
findMap mapping list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case mapping first of
                Just result ->
                    Just result

                Nothing ->
                    findMap mapping rest


{-| Find the first element that satisfies a predicate
and return the element with its direct neighbours.

The type of the return value uses a `Maybe` for the neighbours
(they may not exist if the matched element at the start or at the end of the list)
as well as a `Maybe` for the return value at whole (no element may satisfy the predicate).

-}
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


{-| Lift an ordering on the element type to a list of that type.
-}



-- TODO: Suggest for elm-community/list-extra, and posssibly also for matthewsj/elm-ordering


lexicalOrdering : (a -> a -> Order) -> List a -> List a -> Order
lexicalOrdering compareElements listL listR =
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
                    lexicalOrdering compareElements tailL tailR


{-| Lift an ordering on a type to a Maybe of that type.
-}



-- TODO: Suggest for elm-community/maybe-extra, and posssibly also for matthewsj/elm-ordering


maybeOrdering : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
maybeOrdering compareBaseType maybeL maybeR =
    case ( maybeL, maybeR ) of
        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, Just _ ) ->
            LT

        ( Just _, Nothing ) ->
            GT

        ( Just l, Just r ) ->
            compareBaseType l r


{-| Map elements while they map to a Just.

Return value is the mapped prefix of the list,
and a Bool to indicate if elements were dropped due to mapping an element to Nothing.

-}
mapWhile : (a -> Maybe b) -> List a -> ( Bool, List b )
mapWhile mapping list =
    List.foldl
        (\element ( isContinuous, resultSoFar ) ->
            if isContinuous then
                case mapping element of
                    Just value ->
                        ( True, value :: resultSoFar )

                    Nothing ->
                        ( False, resultSoFar )

            else
                ( False, resultSoFar )
        )
        ( True, [] )
        list
        |> Tuple.mapSecond List.reverse


{-| Map elements while they map to a Just.

On the first mapping to Nothing add a placeholder
(think of an ellipsis) to the result and terminate.

-}
mapEllipsis : b -> (a -> Maybe b) -> List a -> List b
mapEllipsis placeholderEllipsis mapping list =
    List.foldl
        (\element ( isContinuous, resultSoFar ) ->
            if isContinuous then
                case mapping element of
                    Just value ->
                        ( True, value :: resultSoFar )

                    Nothing ->
                        ( False, placeholderEllipsis :: resultSoFar )

            else
                ( False, resultSoFar )
        )
        ( True, [] )
        list
        |> Tuple.second
        |> List.reverse


{-| Check the success-value of a `RemoteData` and conditionally turn it into a failure-value.
-}
remoteDataCheck : (a -> Maybe e) -> RemoteData e a -> RemoteData e a
remoteDataCheck check remoteData =
    case remoteData of
        RemoteData.Success value ->
            case check value of
                Nothing ->
                    remoteData

                Just error ->
                    RemoteData.Failure error

        _ ->
            remoteData


{-| Map the success-value of a `RemoteData` or replace it with a failure-value.
-}
remoteDataMapFallible : (a -> Result e a) -> RemoteData e a -> RemoteData e a
remoteDataMapFallible mapping remoteData =
    case remoteData of
        RemoteData.Success value ->
            mapping value |> RemoteData.fromResult

        _ ->
            remoteData


{-| Create a custom `Sorter` by defining how to order two values.

Used for [`Sort.Dict`](/packages/rtfeldman/elm-sorter-experiment/latest/Sort-Dict).

-}
sorter : Ordering a -> Sorter a
sorter ordering =
    Sort.custom ordering


{-| A string containing a no ["no-break space"](https://en.wikipedia.org/wiki/Non-breaking_space).

Can be used within an otherwise empty `<div>` element in order to prevent collapsing the rendered space.

-}
noBreakSpace : String
noBreakSpace =
    String.fromChar (Char.fromCode 160)


{-| Detect `change` events for things like text fields.

Currently not defined in `elm/html` version 1.0.0.

[From MDN](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event):

> "The `change` event is fired for `<input>`, `<select>`, and `<textarea>` elements
> when an alteration to the element's value is committed by the user.
> Unlike the `input` event, the `change` event is not necessarily fired
> for each alteration to an element's value."

Caution: In most cases you will want to use `Html.Events.onInput` instead.
If you don't use `onInput` events to get each edit of the field then the field may get reset
to the previous value through `Html.Attributes.value` in each rendering,
i.e. on any Msg that the app processes, e.g. a HTTP response.

Use the `onChange` only if this behaviour is acceptable.

-}
onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.on "change"
        (Json.Decode.map tagger Html.Events.targetValue)
