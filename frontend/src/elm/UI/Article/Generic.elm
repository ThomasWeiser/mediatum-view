module UI.Article.Generic exposing
    ( Context
    , Model
    , Msg
    , initialModel
    , update
    , view
    )

{-|

@docs Context
@docs Model
@docs Msg
@docs initialModel
@docs update
@docs view

-}

import Cache exposing (Cache)
import Cache.Derive
import Html exposing (Html)
import RemoteData exposing (RemoteData(..))
import Types exposing (DocumentIdFromSearch, NodeType(..))
import Types.Config exposing (Config)
import Types.Id as Id exposing (NodeId)
import UI.Icons
import Utils
import Utils.Html


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , genericParameters : Maybe ( NodeId, Maybe DocumentIdFromSearch )
    }


{-| -}
type alias Model =
    ()


{-| -}
type alias Msg =
    Never


{-| -}
initialModel : Model
initialModel =
    ()


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    let
        remoteDataMessage =
            -- Find the reason why we have a GenericPresentation
            case context.genericParameters of
                Nothing ->
                    Cache.Derive.getRootFolder context.cache
                        |> RemoteData.map (always "Going to show the root folder")

                Just ( nodeId, Nothing ) ->
                    Cache.Derive.getNodeType context.cache nodeId
                        |> Cache.Derive.asDerivedData
                        |> Utils.remoteDataCheck
                            (\nodeType ->
                                Maybe.map Cache.Derive.CacheDerivationError <|
                                    if nodeType == NodeIsNeither then
                                        Just <|
                                            "Node "
                                                ++ Id.toString nodeId
                                                ++ " is neither a folder nor a document"

                                    else
                                        Nothing
                            )
                        |> RemoteData.map
                            (always <|
                                "Going to show the folder or document "
                                    ++ Id.toString nodeId
                            )

                Just ( nodeIdOne, Just documentIdFromSearch ) ->
                    let
                        nodeIdTwo =
                            Id.asNodeId documentIdFromSearch.id
                    in
                    RemoteData.map2 Tuple.pair
                        (Cache.Derive.getNodeType context.cache nodeIdOne |> Cache.Derive.asDerivedData)
                        (Cache.Derive.getNodeType context.cache nodeIdTwo |> Cache.Derive.asDerivedData)
                        |> Utils.remoteDataCheck
                            (\( nodeTypeOne, nodeTypeTwo ) ->
                                Maybe.map Cache.Derive.CacheDerivationError <|
                                    case ( nodeTypeOne, nodeTypeTwo ) of
                                        ( NodeIsFolder _, NodeIsDocument ) ->
                                            Nothing

                                        ( NodeIsFolder _, _ ) ->
                                            Just <|
                                                "Node "
                                                    ++ Id.toString nodeIdTwo
                                                    ++ " is not a document"

                                        ( _, _ ) ->
                                            Just <|
                                                "Node "
                                                    ++ Id.toString nodeIdOne
                                                    ++ " is not a folder"
                            )
                        |> RemoteData.map
                            (always <|
                                "Going to show document "
                                    ++ Id.toString nodeIdOne
                                    ++ " in folder"
                                    ++ Id.toString nodeIdTwo
                            )
    in
    Html.div [] <|
        case remoteDataMessage of
            NotAsked ->
                [ UI.Icons.spinner ]

            Loading ->
                [ UI.Icons.spinner ]

            Success notification ->
                [ Html.text notification ]

            Failure cacheError ->
                [ Utils.Html.viewCacheError cacheError ]
