module Types exposing
    ( DocumentResult
    , DocumentsPage
    , WindowPage
    )

import Types.Document exposing (Document)


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
