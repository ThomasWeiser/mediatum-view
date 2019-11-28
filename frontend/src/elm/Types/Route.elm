module Types.Route exposing
    ( defaultLimit
    , defaultFtsSorting
    , Route
    , RoutePath(..)
    , RouteParameters
    , initHome
    , initDocumentInFolder
    )

{-| For each correctly formed URL there is a unique representation in terms of the type [`Route`](#Route).

Parsing and stringifying is defined in [`Types.Route.Url`](Types-Route-Url).

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
import Types.SearchTerm exposing (SearchTerm, SetOfSearchTerms)
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
    , filterByTitle : SetOfSearchTerms
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
    , filterByTitle = Types.SearchTerm.emptySet
    , offset = 0
    , limit = defaultLimit
    }
