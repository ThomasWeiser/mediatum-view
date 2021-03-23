module Types.Route exposing
    ( Route
    , RoutePath(..)
    , RouteParameters
    , initHome
    , initDocumentInFolder
    , sanitize
    )

{-| Each correctly formed URL corresponds to a unique value of type [`Route`](#Route).

Parsing URLs and stringifying routes are defined in [`Types.Route.Url`](Types-Route-Url).

@docs Route
@docs RoutePath
@docs RouteParameters

@docs initHome
@docs initDocumentInFolder

@docs sanitize

-}

import Types.Config exposing (Config)
import Types.Config.FacetAspectConfig as FacetAspect
import Types.Config.FtsAspectConfig as FtsAspect
import Types.FilterList as FilterList
import Types.Id exposing (DocumentId, FolderId, NodeId)
import Types.Selection as Selection exposing (FacetFilters, FtsFilters, GlobalFts, Sorting(..))


{-| -}
type alias Route =
    { path : RoutePath
    , parameters : RouteParameters
    }


{-| -}
type RoutePath
    = NoId
    | OneId NodeId
    | TwoIds NodeId NodeId


{-| -}
type alias RouteParameters =
    { globalFts : GlobalFts
    , sorting : Sorting
    , ftsFilters : FtsFilters
    , facetFilters : FacetFilters
    , limit : Int
    }


{-| -}
initHome : Config -> Route
initHome config =
    { path = NoId
    , parameters = emptyParameters config
    }


{-| A route to a document within a folder without any further parameters.
-}
initDocumentInFolder : Config -> FolderId -> DocumentId -> Route
initDocumentInFolder config folderId documentId =
    { path = TwoIds (Types.Id.asNodeId folderId) (Types.Id.asNodeId documentId)
    , parameters = emptyParameters config
    }


emptyParameters : Config -> RouteParameters
emptyParameters config =
    { globalFts = Nothing
    , sorting = config.defaultSorting
    , ftsFilters = Selection.initFtsFilters
    , facetFilters = Selection.initFacetFilters
    , limit = config.defaultLimit
    }


{-| Make sure the route does not contain any unwanted details.
In particular, make sure that the aspects used for fts filters or facet filters are valid.
-}
sanitize : Config -> Route -> Route
sanitize config route =
    let
        parameters =
            route.parameters
    in
    { path = route.path
    , parameters =
        if config.serverConfigAdopted then
            { parameters
                | ftsFilters =
                    parameters.ftsFilters
                        |> FilterList.filterAspects (FtsAspect.aspects config.ftsAspects)
                , facetFilters =
                    parameters.facetFilters
                        |> FilterList.filterAspects (FacetAspect.aspects config.facetAspects)
            }

        else
            parameters
    }
