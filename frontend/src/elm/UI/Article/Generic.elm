module UI.Article.Generic exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Data.Cache as Cache
import Data.Derive
import Html exposing (Html)
import Icons
import RemoteData exposing (RemoteData(..))
import Types.Id as Id exposing (NodeId)
import Types.NodeType exposing (NodeType(..))
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
                    Data.Derive.getRootFolder context.cache
                        |> RemoteData.map (always "Going to show the root folder")

                Just ( nodeId, Nothing ) ->
                    Data.Derive.getNodeType context.cache nodeId
                        |> Data.Derive.asDerivedData
                        |> Utils.remoteDataCheck
                            (\nodeType ->
                                Maybe.map Cache.CacheDataError <|
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

                Just ( nodeIdOne, Just nodeIdTwo ) ->
                    RemoteData.map2 Tuple.pair
                        (Data.Derive.getNodeType context.cache nodeIdOne |> Data.Derive.asDerivedData)
                        (Data.Derive.getNodeType context.cache nodeIdTwo |> Data.Derive.asDerivedData)
                        |> Utils.remoteDataCheck
                            (\( nodeTypeOne, nodeTypeTwo ) ->
                                Maybe.map Cache.CacheDataError <|
                                    case ( nodeTypeOne, nodeTypeTwo ) of
                                        ( NodeIsFolder _, NodeIsDocument ) ->
                                            Nothing

                                        ( NodeIsFolder _, _ ) ->
                                            Just <|
                                                "Node "
                                                    ++ Id.toString nodeIdOne
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
                [ Icons.spinner ]

            Loading ->
                [ Icons.spinner ]

            Success notification ->
                [ Html.text notification ]

            Failure cacheError ->
                [ Html.div [] [ Html.text "Oops, something went wrong here ..." ]
                , Html.small [] [ Html.text (Cache.errorToString cacheError) ]
                ]
