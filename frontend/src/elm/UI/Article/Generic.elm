module UI.Article.Generic exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Data.Cache as Cache
import Html exposing (Html)
import Icons
import RemoteData exposing (RemoteData(..))
import Types exposing (NodeType(..))
import Types.NodeId as NodeId exposing (NodeId)
import Utils


type alias Context =
    { cache : Cache.Model
    , nodeIds : Maybe ( NodeId, Maybe NodeId )
    }


type alias Model =
    ()


type alias Msg =
    Never


initialModel : Model
initialModel =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Context -> Model -> Html Msg
view context model =
    let
        remoteDataMessage =
            -- Find the reason why we have a GenericPresentation
            case context.nodeIds of
                Nothing ->
                    Cache.getRootFolder context.cache
                        |> RemoteData.map (always "Going to show the root folder")

                Just ( nodeId, Nothing ) ->
                    Cache.getNodeType context.cache nodeId
                        |> Cache.asDerivedData
                        |> Utils.remoteDataCheck
                            (\nodeType ->
                                Maybe.map Cache.CacheDataError <|
                                    if nodeType == NodeIsNeither then
                                        Just <|
                                            "Node "
                                                ++ NodeId.toString nodeId
                                                ++ " is neither a folder nor a document"

                                    else
                                        Nothing
                            )
                        |> RemoteData.map
                            (always <|
                                "Going to show the folder or document "
                                    ++ NodeId.toString nodeId
                            )

                Just ( nodeIdOne, Just nodeIdTwo ) ->
                    RemoteData.map2 Tuple.pair
                        (Cache.getNodeType context.cache nodeIdOne |> Cache.asDerivedData)
                        (Cache.getNodeType context.cache nodeIdTwo |> Cache.asDerivedData)
                        |> Utils.remoteDataCheck
                            (\( nodeTypeOne, nodeTypeTwo ) ->
                                Maybe.map Cache.CacheDataError <|
                                    case ( nodeTypeOne, nodeTypeTwo ) of
                                        ( NodeIsFolder _, NodeIsDocument ) ->
                                            Nothing

                                        ( NodeIsFolder _, _ ) ->
                                            Just <|
                                                "Node "
                                                    ++ NodeId.toString nodeIdOne
                                                    ++ " is not a document"

                                        ( _, _ ) ->
                                            Just <|
                                                "Node "
                                                    ++ NodeId.toString nodeIdOne
                                                    ++ " is not a folder"
                            )
                        |> RemoteData.map
                            (always <|
                                "Going to show document "
                                    ++ NodeId.toString nodeIdOne
                                    ++ " in folder"
                                    ++ NodeId.toString nodeIdTwo
                            )
    in
    Html.div [] <|
        case remoteDataMessage of
            NotAsked ->
                [ Icons.spinner ]

            Loading ->
                [ Icons.spinner ]

            Success notification ->
                [ Html.text notification ]

            Failure cacheError ->
                [ Html.div [] [ Html.text "Oops, something went wrong here ..." ]
                , Html.small [] [ Html.text (Cache.errorToString cacheError) ]
                ]
