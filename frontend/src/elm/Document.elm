module Document exposing
    ( attributeValue
    , init
    )

import Data.Types exposing (Document, DocumentAttribute, DocumentId)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra


init : DocumentId -> String -> String -> List DocumentAttribute -> Document
init id metadatatypeName name attributes =
    { id = id
    , name = name
    , metadatatypeName = metadatatypeName
    , attributes = attributes
    }


attributeValue : String -> Document -> Maybe String
attributeValue key document =
    List.Extra.find
        (\attribute -> attribute.field == key)
        document.attributes
        |> Maybe.map (.value >> Maybe.withDefault "")
