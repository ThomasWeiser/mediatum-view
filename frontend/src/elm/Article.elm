module Article exposing
    ( Model
    , Msg
    , Return(..)
    , folderCountsForQuery
    , initWithQuery
    , initialModelEmpty
    , needs
    , update
    , view
    )

import Article.Collection
import Article.Details
import Article.Directory
import Article.Empty
import Article.Fts
import Data.Cache as Cache exposing (ApiData)
import Data.Types
    exposing
        ( Document
        , DocumentId
        , Filter
        , FolderCounts
        , FolderType(..)
        , FtsSorting(..)
        , SearchMethod(..)
        )
import Data.Utils
import Html exposing (Html)
import Html.Attributes
import Query exposing (Query)
import Query.Filter
import Query.Filters
import RemoteData
import Tree
import Utils


type alias Context =
    { cache : Cache.Model
    , query : Query
    }


type Return
    = NoReturn
    | MapQuery (Query -> Query)
    | UpdateCacheWithModifiedDocument Document


type alias Model =
    { content : Content
    }


type Content
    = EmptyModel Article.Empty.Model
    | CollectionModel Article.Collection.Model
    | DirectoryModel Article.Directory.Model
    | FtsModel Article.Fts.Model
    | DetailsModel Query.DetailsQuery Article.Details.Model


type Msg
    = EmptyMsg Article.Empty.Msg
    | CollectionMsg Article.Collection.Msg
    | DirectoryMsg Article.Directory.Msg
    | FtsMsg Article.Fts.Msg
    | DetailsMsg Article.Details.Msg


initialModelEmpty : Model
initialModelEmpty =
    { content = EmptyModel Article.Empty.initialModel }


initWithQuery : Context -> ( Model, Cmd Msg )
initWithQuery context =
    case context.query of
        Query.OnDetails detailsQuery ->
            let
                subModel =
                    Article.Details.initialModel
                        { cache = context.cache
                        , detailsQuery = detailsQuery
                        }
            in
            ( { content = DetailsModel detailsQuery subModel }
            , Cmd.none
            )

        Query.OnFolder folderQuery ->
            case folderQuery.folder.type_ of
                FolderIsCollection ->
                    let
                        ( subModel, subCmd ) =
                            Article.Collection.init ()
                    in
                    ( { content = CollectionModel subModel }
                    , Cmd.map CollectionMsg subCmd
                    )

                FolderIsDirectory ->
                    let
                        subModel =
                            Article.Directory.initialModel
                                { cache = context.cache
                                , folderQuery = folderQuery
                                }
                    in
                    ( { content = DirectoryModel subModel }
                    , Cmd.none
                    )

        Query.OnFts ftsQuery ->
            let
                subModel =
                    Article.Fts.initialModel
                        { cache = context.cache
                        , ftsQuery = ftsQuery
                        }
            in
            ( { content = FtsModel subModel }
            , Cmd.none
            )


needs : Query -> Cache.Needs
needs query =
    case query of
        Query.OnDetails detailsQuery ->
            Cache.NeedDocument detailsQuery.documentId

        Query.OnFolder folderQuery ->
            -- TODO: We currently don't observe the needs of an Iterator
            let
                selection =
                    { scope = folderQuery.folder.id
                    , searchMethod = SelectByFolderListing
                    , filters = folderQuery.filters
                    }
            in
            Cache.NeedListOfNeeds
                [ Cache.NeedDocumentsPage
                    selection
                    folderQuery.window
                , -- TODO: Currently we request the folderCounts in parallel. It should be sequentially after getting the page results
                  Cache.NeedFolderCounts
                    selection
                ]

        Query.OnFts ftsQuery ->
            let
                selection =
                    { scope = ftsQuery.folder.id
                    , searchMethod =
                        SelectByFullTextSearch
                            ftsQuery.searchTerm
                            (case ftsQuery.sorting of
                                Query.ByRank ->
                                    FtsByRank

                                Query.ByDate ->
                                    FtsByDate
                            )
                    , filters = ftsQuery.filters
                    }
            in
            Cache.NeedListOfNeeds
                [ Cache.NeedDocumentsPage
                    selection
                    ftsQuery.window
                , -- TODO: Currently we request the folderCounts in parallel. It should be sequentially after getting the page results
                  Cache.NeedFolderCounts
                    selection
                ]


folderCountsForQuery : Context -> FolderCounts
folderCountsForQuery context =
    case context.query of
        Query.OnDetails detailsQuery ->
            Data.Utils.folderCountsInit

        Query.OnFolder folderQuery ->
            let
                selection =
                    { scope = folderQuery.folder.id
                    , searchMethod = SelectByFolderListing
                    , filters = folderQuery.filters
                    }
            in
            Cache.get context.cache.folderCounts selection
                |> RemoteData.withDefault Data.Utils.folderCountsInit

        Query.OnFts ftsQuery ->
            let
                selection =
                    { scope = ftsQuery.folder.id
                    , searchMethod =
                        SelectByFullTextSearch
                            ftsQuery.searchTerm
                            (case ftsQuery.sorting of
                                Query.ByRank ->
                                    FtsByRank

                                Query.ByDate ->
                                    FtsByDate
                            )
                    , filters = ftsQuery.filters
                    }
            in
            Cache.get context.cache.folderCounts selection
                |> RemoteData.withDefault Data.Utils.folderCountsInit


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case ( msg, model.content, context.query ) of
        ( EmptyMsg subMsg, EmptyModel subModel, _ ) ->
            let
                ( subModel1, subCmd ) =
                    Article.Empty.update subMsg subModel
            in
            ( { model | content = EmptyModel subModel1 }
            , Cmd.map EmptyMsg subCmd
            , NoReturn
            )

        ( CollectionMsg subMsg, CollectionModel subModel, _ ) ->
            let
                ( subModel1, subCmd ) =
                    Article.Collection.update subMsg subModel
            in
            ( { model | content = CollectionModel subModel1 }
            , Cmd.map CollectionMsg subCmd
            , NoReturn
            )

        ( DirectoryMsg subMsg, DirectoryModel subModel, Query.OnFolder folderQuery ) ->
            let
                ( subModel1, subCmd, documentSelection ) =
                    Article.Directory.update
                        { cache = context.cache
                        , folderQuery = folderQuery
                        }
                        subMsg
                        subModel
            in
            case documentSelection of
                Article.Directory.NoReturn ->
                    ( { model | content = DirectoryModel subModel1 }
                    , Cmd.map DirectoryMsg subCmd
                    , NoReturn
                    )

                Article.Directory.SetWindow window ->
                    ( model
                    , Cmd.none
                    , MapQuery <|
                        Query.setWindow window
                    )

                Article.Directory.ShowDocument documentId ->
                    ( model
                    , Cmd.none
                    , MapQuery <|
                        always <|
                            Query.OnDetails
                                { folder = folderQuery.folder
                                , documentId = documentId

                                -- KL: keep filters from original query
                                , filters = folderQuery.filters
                                }
                    )

        ( FtsMsg subMsg, FtsModel subModel, Query.OnFts ftsQuery ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    Article.Fts.update
                        { cache = context.cache
                        , ftsQuery = ftsQuery
                        }
                        subMsg
                        subModel
            in
            case subReturn of
                Article.Fts.NoReturn ->
                    ( { model | content = FtsModel subModel1 }
                    , Cmd.map FtsMsg subCmd
                    , NoReturn
                    )

                Article.Fts.SetWindow window ->
                    ( model
                    , Cmd.none
                    , MapQuery <|
                        Query.setWindow window
                    )

                Article.Fts.ShowDocument documentId ->
                    ( model
                    , Cmd.none
                    , MapQuery <|
                        always <|
                            Query.OnDetails
                                { folder = ftsQuery.folder
                                , documentId = documentId

                                -- KL: keep filters from original query
                                , filters = ftsQuery.filters
                                }
                    )

        ( DetailsMsg subMsg, DetailsModel detailsQuery subModel, _ ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    Article.Details.update
                        { cache = context.cache
                        , detailsQuery = detailsQuery
                        }
                        subMsg
                        subModel
            in
            ( { model | content = DetailsModel detailsQuery subModel1 }
            , Cmd.map DetailsMsg subCmd
            , case subReturn of
                Article.Details.NoReturn ->
                    NoReturn

                Article.Details.UpdateCacheWithModifiedDocument document ->
                    UpdateCacheWithModifiedDocument document
            )

        _ ->
            -- Message doesn't match model; shouldn't never happen
            -- Or model doesn't match query-context; TODO: Can this happen?
            ( model, Cmd.none, NoReturn )


view : Tree.Model -> Context -> Model -> Html Msg
view tree context model =
    Html.article
        [ Html.Attributes.class "article" ]
        [ Html.div
            [ Html.Attributes.class "breadcrumbs" ]
            [ Tree.viewBreadcrumbs
                { cache = context.cache }
                tree
                (Query.getFolder context.query |> .id)
            ]
        , Query.view context.query
            |> Html.map never
        , viewContent context model
        ]


viewContent : Context -> Model -> Html Msg
viewContent context model =
    case ( model.content, context.query ) of
        ( EmptyModel subModel, _ ) ->
            Article.Empty.view subModel
                |> Html.map EmptyMsg

        ( CollectionModel subModel, Query.OnFolder folderQuery ) ->
            Article.Collection.view
                { folderQuery = folderQuery }
                subModel
                |> Html.map CollectionMsg

        ( DirectoryModel subModel, Query.OnFolder folderQuery ) ->
            Article.Directory.view
                { cache = context.cache
                , folderQuery = folderQuery
                }
                subModel
                |> Html.map DirectoryMsg

        ( FtsModel subModel, Query.OnFts ftsQuery ) ->
            Article.Fts.view
                { cache = context.cache
                , ftsQuery = ftsQuery
                }
                subModel
                |> Html.map FtsMsg

        ( DetailsModel detailsQuery subModel, _ ) ->
            Article.Details.view
                { cache = context.cache
                , detailsQuery = detailsQuery
                }
                subModel
                |> Html.map DetailsMsg

        _ ->
            -- Model doesn't match query-context; TODO: Can this happen?
            Html.text ""
