module Entities.PageSequence exposing (PageSequence)

import Entities.DocumentResults exposing (DocumentsPage)
import Types.ApiData exposing (ApiData)


{-| Listings of documents may get queryied in several consecutive pages,
e.g. by a UI button to load more results.

The type `PageSequence` represents such a sequence of pages
as it is managed by the cache.

@docs PageSequence

-}
type alias PageSequence =
    List ( Int, ApiData DocumentsPage )
