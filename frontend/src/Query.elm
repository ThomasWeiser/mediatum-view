module Query exposing
    ( FtsOptions
    , FtsSearchDomain(..)
    , FtsSearchLanguage(..)
    , Fts
    , Query(..)
    , exampleFilters
    , ftsOptionsDomainToString
    , ftsOptionsFromLabel
    , ftsOptionsLanguageToString
    , ftsOptionsToLabel
    , filtersToAttributeTests
    , getFilters
    , getFolder
    , isFts
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


type Query
    = FtsQuery Fts


type alias Fts =
    { folder : Folder
    , filters : List Filter
    , options : FtsOptions
    , searchTerm : String
    }


type alias FtsOptions =
    { domain : FtsSearchDomain
    , language : FtsSearchLanguage
    }


type FtsSearchDomain
    = SearchAttributes
    | SearchFulltext


type FtsSearchLanguage
    = English
    | German


getFolder : Query -> Folder
getFolder query =
    case query of
        FtsQuery { folder } ->
            folder


getFilters : Query -> List Filter
getFilters query =
    case query of
        FtsQuery { filters } ->
            filters


isFts : Query -> Bool
isFts query =
    case query of
        FtsQuery { searchTerm } ->
            searchTerm /= ""


view : Query -> Html msg
view query =
    Html.div []
        [ viewSearch query
        , viewFilters (getFilters query)
        ]


viewSearch : Query -> Html msg
viewSearch query =
    Html.div [] <|
        if not (isFts query) then
            [ Html.span [] [ Html.text "All Documents" ] ]

        else
            case query of
                FtsQuery { options, searchTerm } ->
                    [ Html.span [] [ Html.text "Search " ]
                    , Html.span [] [ Html.text (ftsOptionsToLabel options) ]
                    , Html.span [] [ Html.text ": \"" ]
                    , Html.span [] [ Html.text searchTerm ]
                    , Html.span [] [ Html.text "\"" ]
                    ]


viewFilters : List Filter -> Html msg
viewFilters filters1 =
    Html.div [] <|
        List.map
            Query.Filter.view
            filters1


filtersToAttributeTests : List Filter -> List Query.Attribute.Test
filtersToAttributeTests filters =
    List.map Query.Filter.toAttributeTest filters


ftsOptionsDomainToString : FtsOptions -> String
ftsOptionsDomainToString options =
    case options.domain of
        SearchAttributes ->
            "attrs"

        SearchFulltext ->
            "fulltext"


ftsOptionsLanguageToString : FtsOptions -> String
ftsOptionsLanguageToString options =
    case options.language of
        English ->
            "english"

        German ->
            "german"


ftsOptionsToLabel : FtsOptions -> String
ftsOptionsToLabel options =
    case ( options.domain, options.language ) of
        ( SearchAttributes, English ) ->
            "All Attributes, using English dictionary"

        ( SearchAttributes, German ) ->
            "All Attributes, using German dictionary"

        ( SearchFulltext, English ) ->
            "Fulltext, using English dictionary"

        ( SearchFulltext, German ) ->
            "Fulltext, using German dictionary"


ftsOptionsFromLabel : String -> Maybe FtsOptions
ftsOptionsFromLabel label =
    case label of
        "All Attributes, using English dictionary" ->
            Just <| FtsOptions SearchAttributes English

        "All Attributes, using German dictionary" ->
            Just <| FtsOptions SearchAttributes German

        "Fulltext, using English dictionary" ->
            Just <| FtsOptions SearchFulltext English

        "Fulltext, using German dictionary" ->
            Just <| FtsOptions SearchFulltext German

        _ ->
            Nothing
