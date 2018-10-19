module Query exposing
    ( Directory
    , Fts
    , FtsOptions
    , FtsSearchDomain(..)
    , FtsSearchLanguage(..)
    , Query(..)
    , emptyQuery
    , exampleFilters
    , filtersToAttributeTests
    , ftsOptionsDomainToString
    , ftsOptionsFromLabel
    , ftsOptionsLanguageToString
    , ftsOptionsToLabel
    , getFilters
    , getFolder
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
    = DirectoryQuery Directory
    | FtsQuery Fts


type alias Directory =
    { folder : Folder
    , filters : List Filter
    }


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


emptyQuery : Query
emptyQuery =
    FtsQuery
        { folder = Folder.dummy
        , filters = []
        , options = { domain = SearchAttributes, language = English }
        , searchTerm = ""
        }


getFolder : Query -> Folder
getFolder query =
    case query of
        FtsQuery { folder } ->
            folder

        DirectoryQuery { folder } ->
            folder


getFilters : Query -> List Filter
getFilters query =
    case query of
        FtsQuery { filters } ->
            filters

        DirectoryQuery { filters } ->
            filters


view : Query -> Html msg
view query =
    Html.div []
        [ viewSearch query
        , viewFilters (getFilters query)
        ]


viewSearch : Query -> Html msg
viewSearch query =
    Html.div [] <|
        case query of
            DirectoryQuery _ ->
                [ Html.span [] [ Html.text "All Documents" ] ]

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
