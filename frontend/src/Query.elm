module Query exposing
    ( FtsSearchDomain(..)
    , FtsSearchLanguage(..)
    , Query
    , SearchType(..)
    , exampleFilter
    , attributeTests
    , searchTypeDomainToString
    , searchTypeFromLabel
    , searchTypeLanguageToString
    , searchTypeToLabel
    )

import Folder exposing (Folder, FolderCounts)
import Query.Attribute
import Query.Filter exposing (Filter)


exampleFilter =
    Query.Filter.YearWithin "2001" "2002"


type alias Query =
    { folder : Folder
    , searchType : SearchType
    , searchString : String
    , filters : List Filter
    }


type SearchType
    = FtsSearch FtsSearchDomain FtsSearchLanguage


type FtsSearchDomain
    = SearchAttributes
    | SearchFulltext


type FtsSearchLanguage
    = English
    | German


attributeTests : Query -> List Query.Attribute.Test
attributeTests query =
    List.map Query.Filter.toAttributeTest query.filters


searchTypeDomainToString : SearchType -> String
searchTypeDomainToString searchType =
    case searchType of
        FtsSearch SearchAttributes _ ->
            "attrs"

        FtsSearch SearchFulltext _ ->
            "fulltext"


searchTypeLanguageToString : SearchType -> String
searchTypeLanguageToString searchType =
    case searchType of
        FtsSearch _ English ->
            "english"

        FtsSearch _ German ->
            "german"


searchTypeToLabel : SearchType -> String
searchTypeToLabel searchType =
    case searchType of
        FtsSearch SearchAttributes English ->
            "All Attributes - English"

        FtsSearch SearchAttributes German ->
            "All Attributes - German"

        FtsSearch SearchFulltext English ->
            "Fulltext - English"

        FtsSearch SearchFulltext German ->
            "Fulltext - German"


searchTypeFromLabel : String -> Maybe SearchType
searchTypeFromLabel label =
    case label of
        "All Attributes - English" ->
            Just <| FtsSearch SearchAttributes English

        "All Attributes - German" ->
            Just <| FtsSearch SearchAttributes German

        "Fulltext - English" ->
            Just <| FtsSearch SearchFulltext English

        "Fulltext - German" ->
            Just <| FtsSearch SearchFulltext German

        _ ->
            Nothing
