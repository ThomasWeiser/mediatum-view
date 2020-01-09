module Types.Route exposing
    ( defaultLimit
    , defaultFtsSorting
    , Route
    , RoutePath(..)
    , RouteParameters
    , initHome
    , initDocumentInFolder
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

-}

import Types.Id exposing (DocumentId, FolderId, NodeId)
import Types.Range exposing (Range)
import Types.SearchTerm exposing (SearchTerm)
import Types.Selection exposing (FtsSorting(..))


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
    , filterByYear : Maybe (Range Int)
    , filterByTitle : Maybe SearchTerm
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
    , filterByYear = Nothing
    , filterByTitle = Nothing
    , offset = 0
    , limit = defaultLimit
    }
