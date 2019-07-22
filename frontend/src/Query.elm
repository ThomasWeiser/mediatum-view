module Query exposing
    ( DetailsQuery
    , FolderQuery
    , FtsQuery
    , Query(..)
    , emptyQuery
    , filtersToAttributeTests
    , getFilters
    , getFolder
    , mapFilters
    , setFolder
    , showFilters
    , stopgapFolder
    , toRoute
    , view
    )

import Document exposing (DocumentId)
import Folder exposing (Folder, FolderCounts)
import Html exposing (Html)
import List.Extra
import Query.Attribute
import Query.Filter as Filter exposing (Filter)
import Query.Filters as Filters exposing (Filters)
import Route exposing (Route)


type Query
    = OnDetails DetailsQuery
    | OnFolder FolderQuery
    | OnFts FtsQuery


type alias DetailsQuery =
    { folder : Folder
    , documentId : DocumentId
    , filters : Filters
    }


type alias FolderQuery =
    { folder : Folder
    , filters : Filters
    }


type alias FtsQuery =
    { folder : Folder
    , filters : Filters
    , searchTerm : String
    }


type Msg
    = RemoveFilter Int


emptyQuery : Query
emptyQuery =
    OnFolder
        { folder = Folder.dummy
        , filters = Filters.none
        }


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
            OnFolder
                { folder = folder
                , filters = getFilters query
                }

        OnFolder subQuery ->
            OnFolder { subQuery | folder = folder }

        OnFts subQuery ->
            OnFts { subQuery | folder = folder }


stopgapFolder : Maybe Folder -> Query -> Query
stopgapFolder maybeFolder query =
    if getFolder query == Folder.dummy then
        setFolder (Maybe.withDefault Folder.dummy maybeFolder) query

    else
        query


getFilters : Query -> Filters
getFilters query =
    case query of
        OnDetails { folder, filters } ->
            filters

        OnFolder { filters } ->
            filters

        OnFts { filters } ->
            filters


mapFilters : (Filters -> Filters) -> Query -> Query
mapFilters mapping query =
    case query of
        OnDetails _ ->
            query

        OnFolder subQuery ->
            OnFolder { subQuery | filters = mapping subQuery.filters }

        OnFts subQuery ->
            OnFts { subQuery | filters = mapping subQuery.filters }


showFilters : Query -> Bool
showFilters query =
    case query of
        OnDetails _ ->
            False

        OnFolder { folder } ->
            not folder.isCollection

        OnFts _ ->
            True


toRoute : Query -> Route
toRoute query =
    case query of
        OnDetails { documentId } ->
            Route.NodeId <| Document.idToInt documentId

        OnFolder { folder } ->
            Route.NodeId <| Folder.idToInt folder.id

        OnFts { folder } ->
            Route.NodeId <| Folder.idToInt folder.id


view : Query -> Html Never
view query =
    Html.div [] <|
        case query of
            OnDetails _ ->
                []

            OnFolder { folder } ->
                if folder.isCollection then
                    []

                else
                    [ Html.div []
                        [ Html.span [] [ Html.text "All Documents" ] ]
                    ]

            OnFts { searchTerm } ->
                [ Html.div []
                    [ Html.span [] [ Html.text "Search: \"" ]
                    , Html.span [] [ Html.text searchTerm ]
                    , Html.span [] [ Html.text "\"" ]
                    ]
                ]


filtersToAttributeTests : Filters -> List Query.Attribute.Test
filtersToAttributeTests filters =
    Filters.toList filters
        |> List.map Filter.toAttributeTest
