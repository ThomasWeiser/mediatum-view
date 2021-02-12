module Types.Route exposing
    ( defaultSorting
    , Route
    , RoutePath(..)
    , RouteParameters
    , initHome
    , initDocumentInFolder
    , sanitize
    )

{-| Each correctly formed URL corresponds to a unique value of type [`Route`](#Route).

Parsing URLs and stringifying routes are defined in [`Types.Route.Url`](Types-Route-Url).

@docs defaultLimit
@docs defaultSorting

@docs Route
@docs RoutePath
@docs RouteParameters

@docs initHome
@docs initDocumentInFolder

@docs sanitize

-}

import Config
import Sort.Dict
import Types.Aspect exposing (Aspect)
import Types.FilterList as FilterList exposing (FilterList)
import Types.Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection as Selection exposing (FacetFilters, FtsFilters, GlobalFts, Sorting(..))
import Types.ServerConfig as ServerConfig exposing (ServerConfig)


{-| -}
defaultSorting : Sorting
defaultSorting =
    ByRank


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
    , offset : Int
    , limit : Int
    }


{-| -}
initHome : ServerConfig.Defaults -> Route
initHome serverConfigDefaults =
    { path = NoId
    , parameters = emptyParameters serverConfigDefaults
    }


{-| A route to a document within a folder without any further parameters.
-}
initDocumentInFolder : ServerConfig.Defaults -> FolderId -> DocumentId -> Route
initDocumentInFolder serverConfigDefaults folderId documentId =
    { path = TwoIds (Types.Id.asNodeId folderId) (Types.Id.asNodeId documentId)
    , parameters = emptyParameters serverConfigDefaults
    }


emptyParameters : ServerConfig.Defaults -> RouteParameters
emptyParameters serverConfigDefaults =
    { globalFts = Nothing
    , sorting = defaultSorting
    , ftsFilters = Selection.initFtsFilters
    , facetFilters = Selection.initFacetFilters
    , offset = 0
    , limit = serverConfigDefaults.pageSize
    }


{-| Make sure the route does not contain any unwanted details.
In particular, make sure that the aspects used for fts filters or facet filters are valid.
-}
sanitize : Route -> Route
sanitize route =
    let
        parameters =
            route.parameters
    in
    { path = route.path
    , parameters =
        { parameters
            | ftsFilters =
                FilterList.filterAspects Config.validFtsAspects parameters.ftsFilters
            , facetFilters =
                FilterList.filterAspects Config.validFacetAspects parameters.facetFilters
        }
    }


keepOnly : List Aspect -> Sort.Dict.Dict Aspect v -> Sort.Dict.Dict Aspect v
keepOnly validAspects =
    Sort.Dict.keepIf
        (\aspect _ ->
            List.member aspect validAspects
        )
