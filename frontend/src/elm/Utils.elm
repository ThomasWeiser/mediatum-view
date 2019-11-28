module Utils exposing
    ( ifElse
    , when
    , tupleAddThird
    , tupleRemoveThird
    , prependIf
    , prependMaybe
    , findMap
    , findAdjacent
    , lexicalOrder
    , remoteDataCheck
    , remoteDataMapFallible
    , sorter
    , noBreakSpace
    , onChange
    )

{-| -- TODO Groups (List, RemoteData, etc)


# Bool

@docs ifElse
@docs when


# Tuple

@docs tupleAddThird
@docs tupleRemoveThird


# List

@docs prependIf
@docs prependMaybe
@docs findMap
@docs findAdjacent
@docs lexicalOrder


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


{-| -}
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


{-| Conditionally add an element to the front of a list.
-}
prependMaybe : Maybe a -> List a -> List a
prependMaybe maybeElement list =
    case maybeElement of
        Nothing ->
            list

        Just element ->
            element :: list


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
and return the element with its direct neighbours,
using `Maybe` for the neighbours as well as the return value at whole.
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


{-| -}



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


{-| -}
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


{-| -}
remoteDataMapFallible : (a -> Result e a) -> RemoteData e a -> RemoteData e a
remoteDataMapFallible mapping remoteData =
    case remoteData of
        RemoteData.Success value ->
            mapping value |> RemoteData.fromResult

        _ ->
            remoteData


{-| -}
sorter : Ordering a -> Sorter a
sorter ordering =
    Sort.custom ordering


{-| -}
noBreakSpace : String
noBreakSpace =
    String.fromChar (Char.fromCode 160)


{-| -}
onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.on "change"
        (Json.Decode.map tagger Html.Events.targetValue)
