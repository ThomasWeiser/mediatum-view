module Query.Filters exposing
    ( insert
    , none
    , remove
    , toList
    )

import Data.Types exposing (Filter, Filters)
import Dict exposing (Dict)
import Query.Filter as Filter


none : Filters
none =
    Dict.empty


insert : Filter -> Filters -> Filters
insert filter filters =
    Dict.insert (Filter.handle filter) filter filters


remove : String -> Filters -> Filters
remove handle filters =
    Dict.remove handle filters


toList : Filters -> List Filter
toList filters =
    Dict.values filters
