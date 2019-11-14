module Types.Document exposing
    ( Attribute
    , Document
    , attributeValue
    , init
    )

import List.Extra
import Types.Id as Id exposing (DocumentId)


type alias Document =
    { id : DocumentId
    , name : String
    , metadatatypeName : String
    , attributes : List Attribute
    }


type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }


init : DocumentId -> String -> String -> List Attribute -> Document
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
