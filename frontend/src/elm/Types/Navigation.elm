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
import Types.Aspect exposing (Aspect)
import Types.FilterList as FilterList
import Types.Id as Id exposing (DocumentId, FolderId)
import Types.Route as Route exposing (Route)
import Types.Selection exposing (FtsFilters, GlobalFts, Sorting)


{-| -}
type Navigation
    = ListOfNavigations (List Navigation)
    | ShowDocument FolderId DocumentId
    | ShowListingWithFolder FolderId
    | ShowListingWithSearchAndFtsFilter GlobalFts FtsFilters Sorting
    | ShowListingWithAddedFacetFilter Aspect String
    | ShowListingWithRemovedFacetFilter Aspect
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
                parameters
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

        ShowListingWithSearchAndFtsFilter globalFts ftsFilters sorting ->
            { listingRoute
                | parameters =
                    { parameters
                        | globalFts = globalFts
                        , sorting = sorting
                        , ftsFilters = ftsFilters
                    }
            }

        ShowListingWithAddedFacetFilter aspect value ->
            { listingRoute
                | parameters =
                    { parameters
                        | facetFilters =
                            FilterList.insert aspect value parameters.facetFilters
                    }
            }

        ShowListingWithRemovedFacetFilter aspect ->
            { listingRoute
                | parameters =
                    { parameters
                        | facetFilters =
                            FilterList.remove aspect parameters.facetFilters
                    }
            }

        SetLimit limit ->
            { route
                | parameters =
                    { parameters | limit = limit }
            }
