module Utils exposing
    ( ifElse
    , ensure
    , when
    , tupleAddThird
    , tupleRemoveThird
    , remoteDataCheck
    , remoteDataMapFallible
    , sorter, maybeOrdering, lexicalOrdering, tupleOrdering
    , noBreakSpace
    , onChange
    )

{-|


# Bool

@docs ifElse
@docs ensure
@docs when


# Tuple

@docs tupleAddThird
@docs tupleRemoveThird


# RemoteData

@docs remoteDataCheck
@docs remoteDataMapFallible


# Ordering

@docs sorter, maybeOrdering, lexicalOrdering, tupleOrdering


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


{-| Lift a value into Maybe depending on a predicate
-}
ensure : (a -> Bool) -> a -> Maybe a
ensure predicate value =
    if predicate value then
        Just value

    else
        Nothing


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


{-| Lift orderings on two types to a Tuple of that types.
-}
tupleOrdering : (a -> a -> Order) -> (b -> b -> Order) -> ( a, b ) -> ( a, b ) -> Order
tupleOrdering compareFirst compareSecond =
    Ordering.byFieldWith compareFirst Tuple.first
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith compareSecond Tuple.second)


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
