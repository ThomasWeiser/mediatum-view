module Document exposing
    ( attributeValue
    , init
    )

import List.Extra
import Types exposing (Document, DocumentAttribute)
import Types.Id as Id exposing (DocumentId)


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
