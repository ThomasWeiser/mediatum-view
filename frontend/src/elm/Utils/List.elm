module Utils.List exposing
    ( consIf
    , findMap
    , findAdjacent
    , findByMapping, filterByMapping, filterByNotMapping, replaceOnMapping, setOnMapping, updateOnMapping
    , mapWhile, mapEllipsis
    , lexicalOrdering
    )

{-|

@docs consIf, appendIf, appendWithIf
@docs findMap
@docs findAdjacent
@docs findByMapping, filterByMapping, filterByNotMapping, replaceOnMapping, setOnMapping, updateOnMapping
@docs mapWhile, mapEllipsis
@docs lexicalOrdering

-}

import List.Extra


{-| Conditionally add an element to the front of a list.
-}
consIf : Bool -> a -> List a -> List a
consIf condition element list =
    if condition then
        element :: list

    else
        list


{-| Conditionally append a list (second parameter) to the front of another list.
-}
appendIf : Bool -> List a -> List a -> List a
appendIf condition conditionalFirstList secondList =
    if condition then
        List.append conditionalFirstList secondList

    else
        secondList


{-| Conditionally append a list (second parameter) to the end of another list.
-}
appendWithIf : Bool -> List a -> List a -> List a
appendWithIf condition conditionalSecondList firstList =
    if condition then
        List.append firstList conditionalSecondList

    else
        firstList


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


{-| Find an element of a list by comparing its mapped elements with a value.

    findByMapping
        .id
        2
        [ { id = 2, data = "two" }, { id = 5, data = "five" } ]
        == { id = 2, data = "two" }

-}
findByMapping : (a -> b) -> b -> List a -> Maybe a
findByMapping mapping mappedValue list =
    List.Extra.find
        (\elem -> mapping elem == mappedValue)
        list


{-| Keep only elements that map to a given value
-}
filterByMapping : (a -> b) -> b -> List a -> List a
filterByMapping mapping mappedValue list =
    List.filter
        (\elem -> mapping elem == mappedValue)
        list


{-| Keep only elements that don't map to a given value
-}
filterByNotMapping : (a -> b) -> b -> List a -> List a
filterByNotMapping mapping mappedValue list =
    List.filter
        (\elem -> mapping elem /= mappedValue)
        list


{-| Replace all values that have the same mapping as the replacement value.
-}
replaceOnMapping : (a -> b) -> a -> List a -> List a
replaceOnMapping mapping replacement list =
    List.Extra.setIf
        (\elem -> mapping elem == mapping replacement)
        replacement
        list


{-| Replace all values that have the same mapping as the replacement value.
If no matching element is found, the new value is added to the end of the list.
-}
setOnMapping : (a -> b) -> a -> List a -> List a
setOnMapping mapping replacement list =
    {- Easy, but not the most efficient implementation -}
    if findByMapping mapping (mapping replacement) list == Nothing then
        List.append list [ replacement ]

    else
        replaceOnMapping mapping replacement list


{-| Find all elements defined by a mapping value.
Update or remove those values by an update function.
If no matching element is found and the update function results in a Just,
then add it as a new value to the end of the list.
-}
updateOnMapping : (Maybe a -> Maybe a) -> (a -> b) -> b -> List a -> List a
updateOnMapping update mapping mappedValue list =
    {- Easy, but not the most efficient implementation -}
    if findByMapping mapping mappedValue list == Nothing then
        case update Nothing of
            Just newElement ->
                List.append list [ newElement ]

            Nothing ->
                list

    else
        List.filterMap
            (\elem ->
                if mapping elem == mappedValue then
                    update (Just elem)

                else
                    Just elem
            )
            list


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
