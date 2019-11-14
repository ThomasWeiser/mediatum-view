module DocumentResult exposing (init)

import Types exposing (DocumentResult)
import Types.Document exposing (Document)


type alias DocumentResult =
    { number : Int
    , distance : Float
    , document : Document
    }


init : Int -> Float -> Document -> DocumentResult
init number distance document =
    { number = number
    , distance = distance
    , document = document
    }
