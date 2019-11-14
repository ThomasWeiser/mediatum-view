module Types.DocumentResultsPage exposing
    ( DocumentResult
    , DocumentsPage
    , WindowPage
    , initDocumentResult
    )

import Types.Document exposing (Document)



-- TODO: Possibly reorganize these types


type alias DocumentsPage =
    WindowPage DocumentResult


type alias WindowPage itemModel =
    { offset : Int
    , hasNextPage : Bool
    , content : List itemModel
    }


type alias DocumentResult =
    { number : Int
    , distance : Float
    , document : Document
    }


initDocumentResult : Int -> Float -> Document -> DocumentResult
initDocumentResult number distance document =
    { number = number
    , distance = distance
    , document = document
    }
