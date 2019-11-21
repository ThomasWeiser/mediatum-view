module Entities.Results exposing
    ( DocumentResult
    , DocumentsPage
    )

import Entities.Document exposing (Document)
import Types exposing (WindowPage)


type alias DocumentResult =
    { number : Int
    , distance : Float
    , document : Document
    }


type alias DocumentsPage =
    WindowPage DocumentResult