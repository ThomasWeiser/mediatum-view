module Query exposing
    ( DetailsQuery
    , FolderQuery
    , FtsQuery
    , FtsSorting(..)
    , Query(..)
    , emptyQuery
    , getFilters
    , getFolder
    , getWindow
    , initialWindow
    , mapFilters
    , setFolder
    , setWindow
    , showFilters
    , stopgapFolder
    , toRoute
    , view
    )

import Data.Types exposing (DocumentId, Filter, Filters, Folder, FolderCounts, Window)
import Document
import Folder
import Html exposing (Html)
import List.Extra
import Query.Attribute
import Query.Filter as Filter
import Query.Filters as Filters
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
    , window : Window
    }


type alias FtsQuery =
    { folder : Folder
    , filters : Filters
    , searchTerm : String
    , sorting : FtsSorting
    , window : Window
    }


type FtsSorting
    = ByRank
    | ByDate


emptyQuery : Query
emptyQuery =
    OnFolder
        { folder = Folder.dummy
        , filters = Filters.none
        , window = initialWindow
        }


initialWindow : Window
initialWindow =
    { offset = 0, limit = 10 }


getWindow : Query -> Window
getWindow query =
    case query of
        OnDetails _ ->
            initialWindow

        OnFolder { window } ->
            window

        OnFts { window } ->
            window


setWindow : Window -> Query -> Query
setWindow window query =
    case query of
        OnDetails _ ->
            query

        OnFolder subQuery ->
            OnFolder { subQuery | window = window }

        OnFts subQuery ->
            OnFts { subQuery | window = window }


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
                , window = initialWindow
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
            folder.type_ == Data.Types.FolderIsDirectory

        OnFts _ ->
            True


toRoute : Query -> Route
toRoute query =
    case query of
        OnDetails { documentId } ->
            documentId
                |> Data.Types.documentIdToInt
                |> Data.Types.nodeIdFromInt
                |> Route.fromOneId

        OnFolder { folder } ->
            folder.id
                |> Data.Types.folderIdToInt
                |> Data.Types.nodeIdFromInt
                |> Route.fromOneId

        OnFts { folder } ->
            folder.id
                |> Data.Types.folderIdToInt
                |> Data.Types.nodeIdFromInt
                |> Route.fromOneId


view : Query -> Html Never
view query =
    Html.div [] <|
        case query of
            OnDetails _ ->
                []

            OnFolder { folder } ->
                case folder.type_ of
                    Data.Types.FolderIsCollection ->
                        []

                    Data.Types.FolderIsDirectory ->
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
