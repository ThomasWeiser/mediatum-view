module DocumentResult exposing
    ( init
    , view
    )

import Data.Types exposing (Document, DocumentId, DocumentResult)
import Document
import Html exposing (Html)


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


view : (DocumentId -> msg) -> DocumentResult -> Html msg
view clickMsg documentResult =
    Document.view
        clickMsg
        (Just documentResult.number)
        documentResult.document
