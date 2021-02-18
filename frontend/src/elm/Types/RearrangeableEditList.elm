module Types.RearrangeableEditList exposing
    ( RearrangeableEditItem, RearrangeableEditList
    , rearrange
    )

{-|

@docs RearrangeableEditItem, RearrangeableEditList
@docs rearrange

-}


{-| -}
type alias RearrangeableEditItem k v =
    ( k, v )


{-| -}
type alias RearrangeableEditList k v =
    List (RearrangeableEditItem k v)


{-| -}
rearrange : (RearrangeableEditItem k v -> Bool) -> RearrangeableEditList k v -> RearrangeableEditList k v -> RearrangeableEditList k v
rearrange keep routeList editList =
    let
        step : RearrangeableEditList k v -> RearrangeableEditList k v -> RearrangeableEditList k v
        step r e =
            case ( r, e ) of
                ( [], [] ) ->
                    []

                ( _, [] ) ->
                    r

                ( [], _ ) ->
                    List.filter
                        (keepAndNotInList keep routeList)
                        e

                ( r1 :: rs, e1 :: es ) ->
                    if isInList editList r1 then
                        if isSameKey r1 e1 then
                            r1 :: step rs es

                        else
                            case ( isInList routeList e1, keep e1 ) of
                                ( True, True ) ->
                                    r1 :: step rs e

                                ( True, False ) ->
                                    r1
                                        :: step rs
                                            (removeKey r1 e)

                                ( False, True ) ->
                                    if isInList es r1 then
                                        e1 :: step r es

                                    else
                                        r1 :: step rs e

                                ( False, False ) ->
                                    step r es

                    else
                        r1 :: step rs e

        result =
            step routeList editList
    in
    result


isInList : RearrangeableEditList k v -> RearrangeableEditItem k v -> Bool
isInList routeList item =
    List.any (\x -> Tuple.first x == Tuple.first item) routeList


isSameKey : RearrangeableEditItem k v -> RearrangeableEditItem k v -> Bool
isSameKey item1 item2 =
    Tuple.first item1 == Tuple.first item2


keepAndNotInList : (RearrangeableEditItem k v -> Bool) -> RearrangeableEditList k v -> RearrangeableEditItem k v -> Bool
keepAndNotInList keep list item =
    keep item
        && not (isInList list item)


removeKey : RearrangeableEditItem k v -> RearrangeableEditList k v -> RearrangeableEditList k v
removeKey item =
    List.filter (isSameKey item >> not)
