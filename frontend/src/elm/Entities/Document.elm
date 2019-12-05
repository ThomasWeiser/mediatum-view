module Entities.Document exposing
    ( Document, Attribute
    , init, attributeValue
    )

{-| The metadata of a document and its attributes

@docs Document, Attribute
@docs init, attributeValue

-}

import List.Extra
import Types.Id exposing (DocumentId)


{-| -}
type alias Document =
    { id : DocumentId
    , name : String
    , metadatatypeName : String
    , attributes : List Attribute
    }


{-| -}
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
