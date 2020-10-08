module Entities.Document exposing
    ( Document, Attribute
    , init, attributeValue
    )

{-| The metadata of a document and its attributes

@docs Document, Attribute
@docs init, attributeValue

-}

import List.Extra
import List.Nonempty exposing (Nonempty)
import Types.Id exposing (DocumentId, FolderId)


{-| -}
type alias Document =
    { id : DocumentId
    , name : String
    , metadatatypeName : String
    , attributes : List Attribute
    , locations : List (Nonempty FolderId)
    }


{-| -}
type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }


{-| -}
init : DocumentId -> String -> String -> List Attribute -> List (Nonempty FolderId) -> Document
init id metadatatypeName name attributes locations =
    { id = id
    , name = name
    , metadatatypeName = metadatatypeName
    , attributes = attributes
    , locations = locations
    }


{-| Lookup an attribute value by field.
-}
attributeValue : String -> Document -> Maybe String
attributeValue key document =
    List.Extra.find
        (\attribute -> attribute.field == key)
        document.attributes
        |> Maybe.map (.value >> Maybe.withDefault "")
