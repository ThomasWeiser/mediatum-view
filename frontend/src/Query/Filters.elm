module Query.Filters exposing
    ( Filters
    , insert
    , none
    , remove
    , toList
    )

import Dict exposing (Dict)
import Query.Filter as Filter exposing (Filter)


type alias Filters =
    Dict String Filter


none : Filters
none =
    Dict.empty


insert : Filter -> Filters -> Filters
insert filter filters =
    Dict.insert (Filter.key filter) filter filters


remove : Filter -> Filters -> Filters
remove filter filters =
    Dict.remove (Filter.key filter) filters


toList : Filters -> List Filter
toList filters =
    Dict.values filters
