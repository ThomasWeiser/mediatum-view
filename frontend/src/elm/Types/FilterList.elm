module Types.FilterList exposing
    ( FilterList
    , init, fromList, toList
    , get, isEmpty
    , insert, remove, update
    , filterAspects
    , ordering
    )

{-|

@docs FilterList
@docs init, fromList, toList
@docs get, isEmpty
@docs insert, remove, update
@docs filterAspects
@docs ordering

-}

import Maybe exposing (Maybe)
import Ordering exposing (Ordering)
import Types.Aspect as Aspect exposing (Aspect)
import Utils
import Utils.List


{-| -}
type alias FilterList v =
    List ( Aspect, v )


{-| -}
init : FilterList v
init =
    []


{-| -}
fromList : List ( Aspect, v ) -> FilterList v
fromList =
    identity


{-| -}
toList : FilterList v -> List ( Aspect, v )
toList =
    identity


{-| -}
get : Aspect -> FilterList v -> Maybe v
get aspect =
    Utils.List.findByMapping
        Tuple.first
        aspect
        >> Maybe.map Tuple.second


{-| -}
isEmpty : FilterList v -> Bool
isEmpty =
    List.isEmpty


{-| -}
filterAspects : List Aspect -> FilterList v -> FilterList v
filterAspects aspects =
    List.filter
        (\( aspect, _ ) ->
            List.member aspect aspects
        )


{-| -}
insert : Aspect -> v -> FilterList v -> FilterList v
insert aspect value =
    Utils.List.setOnMapping
        Tuple.first
        ( aspect, value )


{-| -}
remove : Aspect -> FilterList v -> FilterList v
remove aspect =
    Utils.List.filterByNotMapping
        Tuple.first
        aspect



-- TODO Test


{-| Loopup an aspect in the `FilterList`, and update (i.e. map or insert or remove) its value.
-}
update : Aspect -> (Maybe v -> Maybe v) -> FilterList v -> FilterList v
update aspect updateValue =
    Utils.List.updateOnMapping
        (Maybe.map Tuple.second
            >> updateValue
            >> Maybe.map (Tuple.pair aspect)
        )
        Tuple.first
        aspect


{-| Defines an ordering on `FilterList`, modulo permutations.
-}
ordering : Ordering v -> Ordering (FilterList v)
ordering orderingValue =
    Utils.lexicalOrdering
        (Utils.tupleOrdering
            Aspect.ordering
            orderingValue
        )
