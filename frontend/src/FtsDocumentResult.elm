module FtsDocumentResult
    exposing
        ( FtsDocumentResult
        , init
        , view
        )

import Html exposing (Html)
import Html.Attributes
import Document exposing (Document, DocumentId)


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
