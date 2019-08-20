module Navigation exposing
    ( Navigation(..)
    , alterRoute
    )

import Data.Cache as Cache
import Data.Types exposing (..)
import Dict
import Route exposing (..)
import Set
import String.Extra


type Navigation
    = ListOfNavigations (List Navigation)
    | RoutePath RoutePath
    | FolderId FolderId
    | FtsParameter String FtsSorting
    | SetFilters Filters
    | Offset Int
    | Limit Int


alterRoute : Cache.Model -> Navigation -> Route -> Route
alterRoute cache navigation route =
    let
        parameters =
            route.parameters

        setParameters newParameters =
            { route
                | parameters = newParameters
            }

        setParametersAndResetOffset newParameters =
            { route
                | parameters =
                    { newParameters | offset = 0 }
            }
    in
    case navigation of
        ListOfNavigations listOfNavigations ->
            List.foldl (alterRoute cache) route listOfNavigations

        RoutePath path ->
            { route | path = path }

        FolderId folderId ->
            { route
                | path =
                    let
                        nodeId =
                            folderId |> folderIdToInt |> nodeIdFromInt
                    in
                    case route.path of
                        Route.NoId ->
                            Route.OneId nodeId

                        Route.OneId idOne ->
                            if Cache.getAsFolderId cache idOne == Nothing then
                                Route.TwoIds nodeId idOne

                            else
                                Route.OneId nodeId

                        Route.TwoIds _ idTwo ->
                            Route.TwoIds nodeId idTwo
                , parameters =
                    { parameters | offset = 0 }
            }

        FtsParameter ftsTerm ftsSorting ->
            { parameters
                | ftsTerm = ftsTerm
                , ftsSorting = ftsSorting
            }
                |> setParametersAndResetOffset

        SetFilters filters ->
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
                                    , Set.insert titleSearchTerm accuFilterByTitle
                                    )
                        )
                        ( Nothing, Set.empty )
                        (Dict.values filters)
            in
            { parameters
                | filterByYear = filterByYear
                , filterByTitle = filterByTitle
            }
                |> setParametersAndResetOffset

        Offset offset ->
            { parameters | offset = offset }
                |> setParameters

        Limit limit ->
            { parameters | limit = limit }
                |> setParameters
