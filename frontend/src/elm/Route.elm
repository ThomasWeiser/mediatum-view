module Route exposing
    ( Route
    , RouteParameters
    , RoutePath(..)
    , defaultFtsSorting
    , defaultLimit
    , fromOneId
    , initDocumentInFolder
    , initHome
    )

import Types.Id exposing (DocumentId, FolderId, NodeId)
import Types.Range exposing (Range)
import Types.SearchTerm exposing (SearchTerm, SetOfSearchTerms)
import Types.Selection exposing (FtsSorting(..))


defaultLimit : Int
defaultLimit =
    10


defaultFtsSorting : FtsSorting
defaultFtsSorting =
    FtsByRank


type alias Route =
    { path : RoutePath
    , parameters : RouteParameters
    }


type RoutePath
    = NoId
    | OneId NodeId
    | TwoIds NodeId NodeId


type alias RouteParameters =
    { ftsTerm : Maybe SearchTerm
    , ftsSorting : FtsSorting
    , filterByYear : Maybe (Range Int)
    , filterByTitle : SetOfSearchTerms
    , offset : Int
    , limit : Int
    }


fromOneId : NodeId -> Route
fromOneId nodeId =
    -- TODO: Only for adopting legacy code. To be removed later.
    { path = OneId nodeId
    , parameters = emptyParameters
    }


initDocumentInFolder : FolderId -> DocumentId -> Route
initDocumentInFolder folderId documentId =
    { path = TwoIds (Types.Id.asNodeId folderId) (Types.Id.asNodeId documentId)
    , parameters = emptyParameters
    }


initHome : Route
initHome =
    { path = NoId
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
