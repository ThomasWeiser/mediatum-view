module Entities.DocumentResults exposing
    ( DocumentResult
    , DocumentsPage
    )

{-|

@docs DocumentResult
@docs DocumentsPage

-}

import Entities.Document exposing (Document)
import Types


{-| A document within a list of results.

In addition to the document itself a result contains:

  - the position within the list,
  - a measure (`distance`) of how relevant the document is in relation to the search query.

-}
type alias DocumentResult =
    { number : Int
    , distance : Float
    , document : Document
    }


{-| A [page](Types#WindowPage) of document results as received from the API
-}
type alias DocumentsPage =
    Types.WindowPage DocumentResult
