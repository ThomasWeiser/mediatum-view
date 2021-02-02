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
    { insertOrder : List ( Aspect, v )
    , canonicalOrder : List ( Aspect, v )
    }


{-| -}
init : FilterList v
init =
    { insertOrder = []
    , canonicalOrder = []
    }


canonize : List ( Aspect, v ) -> FilterList v
canonize listInInsertOrder =
    { insertOrder =
        listInInsertOrder
    , canonicalOrder =
        listInInsertOrder
            |> List.sortBy (Tuple.first >> Aspect.toString)
    }


{-| -}
fromList : List ( Aspect, v ) -> FilterList v
fromList list =
    canonize list


{-| -}
toList : FilterList v -> List ( Aspect, v )
toList =
    .insertOrder


{-| -}
get : Aspect -> FilterList v -> Maybe v
get aspect filterList =
    filterList.insertOrder
        |> Utils.List.findByMapping
            Tuple.first
            aspect
        >> Maybe.map Tuple.second


{-| -}
isEmpty : FilterList v -> Bool
isEmpty filterList =
    List.isEmpty filterList.insertOrder


{-| -}
filterAspects : List Aspect -> FilterList v -> FilterList v
filterAspects aspects filterList =
    filterList.insertOrder
        |> List.filter (\( aspect, _ ) -> List.member aspect aspects)
        |> canonize


{-| -}
insert : Aspect -> v -> FilterList v -> FilterList v
insert aspect value filterList =
    filterList.insertOrder
        |> Utils.List.setOnMapping
            Tuple.first
            ( aspect, value )
        |> canonize


{-| -}
remove : Aspect -> FilterList v -> FilterList v
remove aspect filterList =
    filterList.insertOrder
        |> Utils.List.filterByNotMapping
            Tuple.first
            aspect
        |> canonize



-- TODO Test


{-| Loopup an aspect in the `FilterList`, and update (i.e. map or insert or remove) its value.
-}
update : Aspect -> (Maybe v -> Maybe v) -> FilterList v -> FilterList v
update aspect updateValue filterList =
    filterList.insertOrder
        |> Utils.List.updateOnMapping
            (Maybe.map Tuple.second
                >> updateValue
                >> Maybe.map (Tuple.pair aspect)
            )
            Tuple.first
            aspect
        |> canonize


{-| Defines an ordering on `FilterList`, modulo permutations.
-}
ordering : Ordering v -> Ordering (FilterList v)
ordering orderingValue =
    Ordering.byFieldWith
        (Utils.List.lexicalOrdering
            (Utils.tupleOrdering
                Aspect.ordering
                orderingValue
            )
        )
        .canonicalOrder
