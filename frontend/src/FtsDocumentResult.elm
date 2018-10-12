module FtsDocumentResult exposing
    ( FtsDocumentResult
    , init
    , view
    )

import Document exposing (Document, DocumentId)
import Html exposing (Html)


type alias FtsDocumentResult =
    { number : Int
    , distance : Float
    , document : Document
    }


init : Int -> Float -> Document -> FtsDocumentResult
init number distance document =
    { number = number
    , distance = distance
    , document = document
    }


view : (DocumentId -> msg) -> FtsDocumentResult -> Html msg
view clickMsg ftsDocumentResult =
    Document.view
        clickMsg
        (Just ftsDocumentResult.number)
        ftsDocumentResult.document
