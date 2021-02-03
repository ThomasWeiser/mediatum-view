module Types.FtsFilterLine exposing
    ( FtsFilterLine
    , mergeFtsFilterLines
    , isEmptyAndNotInList
    , FtsFilterLines
    )

{-|

@docs FtsFilterLine, FtsFilterLinesx
@docs mergeFtsFilterLines
@docs isEmptyAndNotInList

-}

import Types.Aspect exposing (Aspect)


type alias FtsFilterLine =
    ( Aspect, String )


type alias FtsFilterLines =
    List FtsFilterLine


mergeFtsFilterLines : FtsFilterLines -> FtsFilterLines -> FtsFilterLines
mergeFtsFilterLines fromRoute uiModel =
    let
        step : FtsFilterLines -> FtsFilterLines -> FtsFilterLines
        step r m =
            case ( r, m ) of
                ( [], [] ) ->
                    []

                ( _, [] ) ->
                    r

                ( [], _ ) ->
                    List.filter
                        (isEmptyAndNotInList fromRoute)
                        m

                ( r1 :: rs, m1 :: ms ) ->
                    if isInList uiModel {- oder m ? -} r1 then
                        if isSameAspect r1 m1 then
                            r1 :: step rs ms

                        else
                            case ( isInList fromRoute {- oder r ? -} m1, isEmpty m1 ) of
                                ( True, True ) ->
                                    r1 :: step rs m

                                ( True, False ) ->
                                    r1
                                        :: step rs
                                            (removeAspect r1 m)

                                ( False, True ) ->
                                    if isInList ms r1 then
                                        m1 :: step r ms

                                    else
                                        r1 :: step rs m

                                ( False, False ) ->
                                    step r ms

                    else
                        r1 :: step rs m

        result =
            step fromRoute uiModel

        _ =
            ( Debug.log "fromRoute" fromRoute
            , Debug.log "model0" uiModel
            , Debug.log "model1" result
            )
    in
    result


isEmpty : FtsFilterLine -> Bool
isEmpty line =
    String.isEmpty (Tuple.second line)


isInList : FtsFilterLines -> FtsFilterLine -> Bool
isInList fromRoute line =
    List.any (\x -> Tuple.first x == Tuple.first line) fromRoute


isSameAspect : FtsFilterLine -> FtsFilterLine -> Bool
isSameAspect line1 line2 =
    Tuple.first line1 == Tuple.first line2


isEmptyAndNotInList : FtsFilterLines -> FtsFilterLine -> Bool
isEmptyAndNotInList fromRoute line =
    isEmpty line
        && not (isInList fromRoute line)


removeAspect : FtsFilterLine -> FtsFilterLines -> FtsFilterLines
removeAspect line =
    List.filter (isSameAspect line >> not)
