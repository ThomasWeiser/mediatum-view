module Types.ServerSetup exposing
    ( ServerSetup
    , ServerConfig
    )

{-| Configuration values provided by the server

@docs ServerSetup
@docs ServerConfig

-}

import Types.Config.FacetAspectConfig exposing (FacetAspectConfig)
import Types.Config.FtsAspectConfig exposing (FtsAspectConfig)
import Types.Config.MasksConfig exposing (MasksPurposeServerConfig)
import Types.Id exposing (FolderId)
import Types.Selection as Selection


{-| -}
type alias ServerSetup =
    { config : ServerConfig
    }


{-| -}
type alias ServerConfig =
    { toplevelFolders : Maybe (List FolderId)
    , defaultPageSize : Maybe Int
    , defaultSorting : Maybe Selection.Sorting
    , numberOfFacetValues : Maybe Int
    , staticFtsAspects : Maybe (List FtsAspectConfig)
    , staticFacetAspects : Maybe (List FacetAspectConfig)
    , masksByPurpose : Maybe (List MasksPurposeServerConfig)
    }
