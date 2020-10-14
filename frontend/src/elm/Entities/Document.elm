module Entities.Document exposing
    ( Document, Attribute
    , init, attributeValue
    )

{-| The metadata of a document and its attributes.

@docs Document, Attribute
@docs init, attributeValue

-}

import List.Extra
import Types.Id exposing (DocumentId)


{-| A document as cached either as a detailed document or within a listing of documents.

The `attributes` are relative to a mask, which depends on the context where a `Document` is used.

-}
type alias Document =
    { id : DocumentId
    , name : String
    , metadatatypeName : String
    , attributes : List Attribute
    }


{-| An attribute of an document through the view of a certain mask (e.g. `nodesmall`, `nodebig`).

`field` is the key in the JSON representation of all attributes.

`name` and `with` are given by the applied mask.

-}
type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }


{-| -}
init : DocumentId -> String -> String -> List Attribute -> Document
init id metadatatypeName name attributes =
    { id = id
    , name = name
    , metadatatypeName = metadatatypeName
    , attributes = attributes
    }


{-| Lookup an attribute value by field.
-}
attributeValue : String -> Document -> Maybe String
attributeValue key document =
    List.Extra.find
        (\attribute -> attribute.field == key)
        document.attributes
        |> Maybe.map (.value >> Maybe.withDefault "")
