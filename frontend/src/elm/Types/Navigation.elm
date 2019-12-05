module Types.Navigation exposing
    ( Navigation(..)
    , alterRoute
    )

{-| A navigation is an instruction to alter a given route in response to some user interaction.

@docs Navigation
@docs alterRoute

-}

import Cache exposing (Cache)
import Cache.Derive
import Types.Id as Id exposing (DocumentId, FolderId)
import Types.Route as Route exposing (Route)
import Types.Route.Filter
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (FtsSorting, SetOfFilters)


{-| -}
type Navigation
    = ListOfNavigations (List Navigation)
    | ShowDocument FolderId DocumentId
    | ShowListingWithFolder FolderId
    | ShowListingWithSearch (Maybe SearchTerm) FtsSorting
    | ShowListingWithFilters SetOfFilters
    | SetFolder FolderId
    | SetOffset Int
    | SetLimit Int


{-| Modify the route accordingly.

In some cases this uses knowledge about node types from the cache.

-}
alterRoute : Cache -> Navigation -> Route -> Route
alterRoute cache navigation route =
    let
        listingRoute =
            { path =
                case route.path of
                    Route.NoId ->
                        Route.NoId

                    Route.OneId idOne ->
                        if Cache.Derive.getAsFolderId cache idOne == Nothing then
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
                        (folderId |> Id.asNodeId)
                        (documentId |> Id.asNodeId)
            }

        ShowListingWithFolder folderId ->
            { listingRoute
                | path =
                    Route.OneId
                        (folderId |> Id.asNodeId)
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
            Types.Route.Filter.alterRoute filters listingRoute

        SetFolder folderId ->
            { path =
                case route.path of
                    Route.NoId ->
                        Route.OneId
                            (folderId |> Id.asNodeId)

                    Route.OneId idOne ->
                        case Cache.Derive.getAsDocumentId cache idOne of
                            Nothing ->
                                Route.OneId
                                    (folderId |> Id.asNodeId)

                            Just documentId ->
                                Route.TwoIds
                                    (folderId |> Id.asNodeId)
                                    (documentId |> Id.asNodeId)

                    Route.TwoIds idOne idTwo ->
                        Route.TwoIds
                            (folderId |> Id.asNodeId)
                            idTwo
            , parameters =
                parameters
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
