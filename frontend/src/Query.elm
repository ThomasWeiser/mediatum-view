module Query exposing
    ( DetailsQuery
    , FolderQuery
    , FtsOptions
    , FtsQuery
    , FtsSearchDomain(..)
    , FtsSearchLanguage(..)
    , Msg
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
    , mapFilters
    , setFolder
    , update
    , view
    )

import Document exposing (DocumentId)
import Folder exposing (Folder, FolderCounts)
import Html exposing (Html)
import List.Extra
import Query.Attribute
import Query.Filter exposing (Filter)


exampleFilters =
    [ Query.Filter.YearWithin "2000" "2010"
    , Query.Filter.YearWithin "2001" "2002"
    , Query.Filter.YearWithin "2000" "2010"
    ]


type Query
    = OnDetails DetailsQuery
    | OnFolder FolderQuery
    | OnFts FtsQuery


type alias DetailsQuery =
    { folder : Folder
    , documentId : DocumentId
    }


type alias FolderQuery =
    { folder : Folder
    , filters : List Filter
    }


type alias FtsQuery =
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


type Msg
    = RemoveFilter Int


emptyQuery : Query
emptyQuery =
    OnFolder { folder = Folder.dummy, filters = [] }


getFolder : Query -> Folder
getFolder query =
    case query of
        OnDetails { folder } ->
            folder

        OnFolder { folder } ->
            folder

        OnFts { folder } ->
            folder


setFolder : Folder -> Query -> Query
setFolder folder query =
    case query of
        OnDetails subQuery ->
            OnDetails { subQuery | folder = folder }

        OnFolder subQuery ->
            OnFolder { subQuery | folder = folder }

        OnFts subQuery ->
            OnFts { subQuery | folder = folder }


getFilters : Query -> Maybe (List Filter)
getFilters query =
    case query of
        OnDetails { folder } ->
            Nothing

        OnFolder { filters } ->
            Just filters

        OnFts { filters } ->
            Just filters


mapFilters : (List Filter -> List Filter) -> Query -> Query
mapFilters mapping query =
    case query of
        OnDetails _ ->
            query

        OnFolder subQuery ->
            OnFolder { subQuery | filters = mapping subQuery.filters }

        OnFts subQuery ->
            OnFts { subQuery | filters = mapping subQuery.filters }


update : Msg -> Query -> Query
update msg query =
    case msg of
        RemoveFilter index ->
            mapFilters
                (List.Extra.removeAt index)
                query


view : Query -> Html Msg
view query =
    Html.div [] <|
        case query of
            OnDetails _ ->
                []

            OnFolder { folder, filters } ->
                if folder.isCollection then
                    []

                else
                    [ Html.div []
                        [ Html.span [] [ Html.text "All Documents" ] ]
                    , viewFilters filters
                    ]

            OnFts { filters, options, searchTerm } ->
                [ Html.div []
                    [ Html.span [] [ Html.text "Search " ]
                    , Html.span [] [ Html.text (ftsOptionsToLabel options) ]
                    , Html.span [] [ Html.text ": \"" ]
                    , Html.span [] [ Html.text searchTerm ]
                    , Html.span [] [ Html.text "\"" ]
                    ]
                , viewFilters filters
                ]


viewFilters : List Filter -> Html Msg
viewFilters filters =
    Html.div [] <|
        List.indexedMap
            (\index filter ->
                Query.Filter.view filter
                    |> Html.map
                        (\filterMsg ->
                            case filterMsg of
                                Query.Filter.Remove ->
                                    RemoveFilter index
                        )
            )
            filters


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
