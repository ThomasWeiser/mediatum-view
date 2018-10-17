module Query exposing
    ( FtsSearchDomain(..)
    , FtsSearchLanguage(..)
    , Query
    , SearchType(..)
    , attributeTests
    , exampleFilters
    , searchTypeDomainToString
    , searchTypeFromLabel
    , searchTypeLanguageToString
    , searchTypeToLabel
    , view
    )

import Folder exposing (Folder, FolderCounts)
import Html exposing (Html)
import Query.Attribute
import Query.Filter exposing (Filter)


exampleFilters =
    [ Query.Filter.YearWithin "2000" "2010"
    , Query.Filter.YearWithin "2001" "2002"
    , Query.Filter.YearWithin "2000" "2010"
    ]


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


view : Query -> Html msg
view query =
    Html.div []
        [ viewSearch query
        , viewFilters query.filters
        ]


viewSearch : Query -> Html msg
viewSearch query =
    Html.div [] <|
        if query.searchString == "" then
            [ Html.span [] [ Html.text "All Documents" ] ]

        else
            [ Html.span [] [ Html.text "Search " ]
            , Html.span []
                [ Html.text <|
                    searchTypeToLabel query.searchType
                ]
            , Html.span [] [ Html.text ": \"" ]
            , Html.span [] [ Html.text query.searchString ]
            , Html.span [] [ Html.text "\"" ]
            ]


viewFilters : List Filter -> Html msg
viewFilters filters =
    Html.div [] <|
        List.map
            Query.Filter.view
            filters


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
