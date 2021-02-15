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

@docs defaultLimit
@docs defaultSorting

@docs Route
@docs RoutePath
@docs RouteParameters

@docs initHome
@docs initDocumentInFolder

@docs sanitize

-}

import Constants
import Sort.Dict
import Types.Aspect exposing (Aspect)
import Types.Config as Config exposing (Config)
import Types.FilterList as FilterList exposing (FilterList)
import Types.Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm exposing (SearchTerm)
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
    , offset : Int
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
    , offset = 0
    , limit = config.defaultPageSize
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
                FilterList.filterAspects Constants.validFtsAspects parameters.ftsFilters
            , facetFilters =
                FilterList.filterAspects Constants.validFacetAspects parameters.facetFilters
        }
    }


keepOnly : List Aspect -> Sort.Dict.Dict Aspect v -> Sort.Dict.Dict Aspect v
keepOnly validAspects =
    Sort.Dict.keepIf
        (\aspect _ ->
            List.member aspect validAspects
        )
