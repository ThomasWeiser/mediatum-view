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
import Dict
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
    | ShowListingWithAddedFacetFilter String String
    | ShowListingWithRemovedFacetFilter String
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
                -- Remove the document id if present
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
                parametersWithOffset0
            }

        parameters =
            route.parameters

        parametersWithOffset0 =
            { parameters | offset = 0 }
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
                    { parametersWithOffset0
                        | ftsTerm = maybeFtsTerm
                        , ftsSorting = ftsSorting
                    }
            }

        ShowListingWithFilters filters ->
            Types.Route.Filter.alterRoute filters listingRoute

        ShowListingWithAddedFacetFilter key value ->
            { listingRoute
                | parameters =
                    { parametersWithOffset0
                        | facetFilters =
                            Dict.insert key value parameters.facetFilters
                    }
            }

        ShowListingWithRemovedFacetFilter key ->
            { listingRoute
                | parameters =
                    { parametersWithOffset0
                        | facetFilters =
                            Dict.remove key parameters.facetFilters
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
