module DocumentResult exposing (init)

import Types exposing (Document, DocumentResult)


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
