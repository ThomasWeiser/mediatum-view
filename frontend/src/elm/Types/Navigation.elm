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
import Sort.Dict
import Types.Aspect exposing (Aspect)
import Types.Id as Id exposing (DocumentId, FolderId)
import Types.Route as Route exposing (Route)
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (FtsFilters, FtsSorting)


{-| -}
type Navigation
    = ListOfNavigations (List Navigation)
    | ShowDocument FolderId DocumentId
    | ShowListingWithFolder FolderId
    | ShowListingWithSearchAndFtsFilter (Maybe SearchTerm) FtsSorting FtsFilters
    | ShowListingWithAddedFacetFilter Aspect String
    | ShowListingWithRemovedFacetFilter Aspect
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

        ShowListingWithSearchAndFtsFilter maybeFtsTerm ftsSorting ftsFilters ->
            { listingRoute
                | parameters =
                    { parametersWithOffset0
                        | ftsTerm = maybeFtsTerm
                        , ftsSorting = ftsSorting
                        , ftsFilters = ftsFilters
                    }
            }

        ShowListingWithAddedFacetFilter aspect value ->
            { listingRoute
                | parameters =
                    { parametersWithOffset0
                        | facetFilters =
                            Sort.Dict.insert aspect value parameters.facetFilters
                    }
            }

        ShowListingWithRemovedFacetFilter aspect ->
            { listingRoute
                | parameters =
                    { parametersWithOffset0
                        | facetFilters =
                            Sort.Dict.remove aspect parameters.facetFilters
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
