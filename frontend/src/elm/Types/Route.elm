module Types.Route exposing
    ( defaultLimit
    , defaultFtsSorting
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
@docs defaultFtsSorting

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
import Types.Id exposing (DocumentId, FolderId, NodeId)
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection as Selection exposing (FacetFilters, FtsFilters, FtsSorting(..))


{-| -}
defaultLimit : Int
defaultLimit =
    10


{-| -}
defaultFtsSorting : FtsSorting
defaultFtsSorting =
    FtsByRank


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
    { ftsTerm : Maybe SearchTerm
    , ftsSorting : FtsSorting
    , ftsFilters : FtsFilters
    , facetFilters : FacetFilters
    , offset : Int
    , limit : Int
    }


{-| -}
initHome : Route
initHome =
    { path = NoId
    , parameters = emptyParameters
    }


{-| A route to a document within a folder without any further parameters.
-}
initDocumentInFolder : FolderId -> DocumentId -> Route
initDocumentInFolder folderId documentId =
    { path = TwoIds (Types.Id.asNodeId folderId) (Types.Id.asNodeId documentId)
    , parameters = emptyParameters
    }


emptyParameters : RouteParameters
emptyParameters =
    { ftsTerm = Nothing
    , ftsSorting = defaultFtsSorting
    , ftsFilters = Selection.initFtsFilters
    , facetFilters = Selection.initFacetFilters
    , offset = 0
    , limit = defaultLimit
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
            | ftsFilters = keepOnly Config.validFtsAspects parameters.ftsFilters
            , facetFilters = keepOnly Config.validFacetAspects parameters.facetFilters
        }
    }


keepOnly : List Aspect -> Sort.Dict.Dict Aspect v -> Sort.Dict.Dict Aspect v
keepOnly validAspects =
    Sort.Dict.keepIf
        (\aspect _ ->
            List.member aspect validAspects
        )
