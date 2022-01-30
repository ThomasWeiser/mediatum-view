module Entities.Document exposing
    ( Document, Attribute, SearchMatching
    , init, attributeValue
    , File, Files
    )

{-| The metadata of a document and its attributes.

@docs Document, Attribute, SearchMatching
@docs init, attributeValue

-}

import Entities.Markup exposing (Markup)
import List.Extra
import Types.Id exposing (DocumentId)


{-| A document as cached either as a detailed document or within a listing of documents.

The `attributes` are relative to a mask, which depends on the context where a `Document` is used.

`searchMatching` contains additional search-related annotations on the document
if originating from a fulltext search.

-}
type alias Document =
    { id : DocumentId
    , name : String
    , metadatatypeName : String
    , attributes : List Attribute
    , searchMatching : Maybe SearchMatching
    , files : Maybe Files
    }


{-| An attribute of an document through the view of a certain mask (e.g. `nodesmall`, `nodebig`).

`field` is the key in the JSON representation of all attributes.

`name` and `with` are given by the applied mask.

-}
type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe Markup
    }


{-| For documents that were found in a search: where does the search term occur?
-}
type alias SearchMatching =
    { fulltext : Bool
    , attributes : Bool
    }


{-| A list of associated files
-}
type alias Files =
    List File


{-| Specification of an associated file
-}
type alias File =
    { filetype : String
    , mimetype : String
    }


{-| -}
init :
    DocumentId
    -> String
    -> String
    -> List Attribute
    -> Maybe SearchMatching
    -> Maybe Files
    -> Document
init id metadatatypeName name attributes searchMatching files =
    { id = id
    , name = name
    , metadatatypeName = metadatatypeName
    , attributes = attributes
    , searchMatching = searchMatching
    , files = files
    }


{-| Lookup an attribute value by field.
-}
attributeValue : String -> Document -> Maybe Markup
attributeValue key document =
    List.Extra.find
        (\attribute -> attribute.field == key)
        document.attributes
        |> Maybe.map (.value >> Maybe.withDefault Entities.Markup.empty)
