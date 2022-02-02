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
import String.Format
import Types exposing (DocumentIdFromSearch, NodeType(..))
import Types.Config exposing (Config)
import Types.Id as Id exposing (NodeId)
import Types.Localization as Localization
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
                    (case List.length context.config.toplevelFolderIds of
                        0 ->
                            { en = "There are no known top folders."
                            , de = "Es sind keine Startverzeichnisse bekannt."
                            }

                        1 ->
                            { en = "Going to show the top folder"
                            , de = "Laden des Startverzeichnisses"
                            }

                        _ ->
                            { en = "Going to show the top folders"
                            , de = "Laden der Startverzeichnisse"
                            }
                    )
                        |> Localization.string context.config
                        |> Success

                Just ( nodeId, Nothing ) ->
                    Cache.Derive.getNodeType context.cache nodeId
                        |> Cache.Derive.asDerivedData
                        |> Utils.remoteDataCheck
                            (\nodeType ->
                                Maybe.map Cache.Derive.CacheDerivationError <|
                                    if nodeType == NodeIsNeither then
                                        Localization.string context.config
                                            { en = "Node {{}} is neither a folder nor a document"
                                            , de = "Node {{}} ist weder ein Verzeichnis noch ein Dokument"
                                            }
                                            |> String.Format.value (Id.toString nodeId)
                                            |> Just

                                    else
                                        Nothing
                            )
                        |> RemoteData.map
                            (Localization.string context.config
                                { en = "Going to show the folder or document {{}}"
                                , de = "Anzeige des Verzeichnisses oder Dokuments {{}} wird vorbereitet"
                                }
                                |> String.Format.value (Id.toString nodeId)
                                |> always
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
                                            Localization.string context.config
                                                { en = "Node {{}} is not a document"
                                                , de = "Node {{}} ist kein Dokument"
                                                }
                                                |> String.Format.value (Id.toString nodeIdTwo)
                                                |> Just

                                        ( _, _ ) ->
                                            Localization.string context.config
                                                { en = "Node {{}} is not a folder"
                                                , de = "Node {{}} ist kein Verzeichnis"
                                                }
                                                |> String.Format.value (Id.toString nodeIdOne)
                                                |> Just
                            )
                        |> RemoteData.map
                            (Localization.string context.config
                                { en = "Going to show document {{}} in folder {{}}"
                                , de = "Anzeige des Dokuments {{}} im Verzeichnis {{}} wird vorbereitet"
                                }
                                |> String.Format.value (Id.toString nodeIdOne)
                                |> String.Format.value (Id.toString nodeIdTwo)
                                |> always
                            )
    in
    Html.article [] <|
        case remoteDataMessage of
            NotAsked ->
                [ UI.Icons.icons.spinner ]

            Loading ->
                [ UI.Icons.icons.spinner ]

            Success notification ->
                [ Html.text notification ]

            Failure cacheError ->
                [ Utils.Html.viewCacheError cacheError ]
