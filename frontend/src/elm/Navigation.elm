module Navigation exposing
    ( Navigation(..)
    , alterRoute
    )

import Data.Cache as Cache
import Data.Types exposing (..)
import Data.Types.SearchTerm exposing (SearchTerm)
import Dict
import Route exposing (..)
import String.Extra


type Navigation
    = ListOfNavigations (List Navigation)
    | ShowDocument FolderId DocumentId
    | ShowListingWithFolder FolderId
    | ShowListingWithSearch (Maybe SearchTerm) FtsSorting
    | ShowListingWithFilters Filters
    | SetOffset Int
    | SetLimit Int


alterRoute : Cache.Model -> Navigation -> Route -> Route
alterRoute cache navigation route =
    let
        listingRoute =
            { path =
                case route.path of
                    Route.NoId ->
                        Route.NoId

                    Route.OneId idOne ->
                        if Cache.getAsFolderId cache idOne == Nothing then
                            Route.NoId

                        else
                            Route.OneId idOne

                    Route.TwoIds idOne _ ->
                        Route.OneId idOne
            , parameters =
                { parameters | offset = 0 }
            }

        parameters =
            route.parameters
    in
    case navigation of
        ListOfNavigations listOfNavigations ->
            List.foldl (alterRoute cache) route listOfNavigations

        ShowDocument folderId documentId ->
            { route
                | path =
                    Route.TwoIds
                        (folderId |> folderIdToInt |> nodeIdFromInt)
                        (documentId |> documentIdToInt |> nodeIdFromInt)
            }

        ShowListingWithFolder folderId ->
            { listingRoute
                | path =
                    Route.OneId
                        (folderId |> folderIdToInt |> nodeIdFromInt)
            }

        ShowListingWithSearch maybeFtsTerm ftsSorting ->
            { listingRoute
                | parameters =
                    { parameters
                        | ftsTerm = maybeFtsTerm
                        , ftsSorting = ftsSorting
                    }
            }

        ShowListingWithFilters filters ->
            let
                ( filterByYear, filterByTitle ) =
                    List.foldl
                        (\filter ( accuFilterByYear, accuFilterByTitle ) ->
                            case filter of
                                FilterYearWithin range ->
                                    ( Just range
                                    , accuFilterByTitle
                                    )

                                FilterTitleFts titleSearchTerm ->
                                    ( accuFilterByYear
                                    , Data.Types.SearchTerm.setInsert titleSearchTerm accuFilterByTitle
                                    )
                        )
                        ( Nothing, Data.Types.SearchTerm.emptySet )
                        (Dict.values filters)
            in
            { listingRoute
                | parameters =
                    { parameters
                        | filterByYear = filterByYear
                        , filterByTitle = filterByTitle
                    }
            }

        SetOffset offset ->
            { route
                | parameters =
                    { parameters | offset = offset }
            }

        SetLimit limit ->
            { route
                | parameters =
                    { parameters | limit = limit }
            }
