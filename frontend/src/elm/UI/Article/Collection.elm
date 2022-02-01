module UI.Article.Collection exposing
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
import Entities.Folder as Folder
import Html exposing (Html)
import Html.Parser
import Html.Parser.Util
import RemoteData
import Types.Config as Config exposing (Config)
import Types.Id exposing (FolderId)
import Types.Localization as Localization
import UI.Icons
import Utils.Html


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , folderId : FolderId
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


view : Context -> Model -> Html Msg
view context model =
    case Config.getCollectionPage context.folderId context.config of
        Nothing ->
            viewWithCollectionName context model

        Just page ->
            page
                |> Localization.string context.config
                |> Html.Parser.run
                |> Result.map
                    Html.Parser.Util.toVirtualDom
                |> Result.withDefault
                    [ viewWithCollectionName context model
                    , Html.div []
                        [ Localization.text context.config
                            { en = "Cannot display front page since its content is not valid HTML."
                            , de = "Startseite kann nicht angezeigt werden, da ihr Inhalt kein gültiges HTML ist."
                            }
                        ]
                    ]
                |> Html.div []


viewWithCollectionName : Context -> Model -> Html Msg
viewWithCollectionName context model =
    Html.div []
        [ case Cache.get context.cache.folders context.folderId of
            RemoteData.NotAsked ->
                -- Should never happen
                UI.Icons.spinner

            RemoteData.Loading ->
                UI.Icons.spinner

            RemoteData.Failure error ->
                Utils.Html.viewApiError error

            RemoteData.Success folder ->
                Html.h3 [] <|
                    if Folder.isRoot folder then
                        [ Localization.text context.config
                            { en = "Front page for root of all collections"
                            , de = "Startseite für alle Kollektionen"
                            }
                        ]

                    else
                        [ Localization.text context.config
                            { en = "Front page for collection"
                            , de = "Startseite für Kollektion"
                            }
                        , Html.text " \""
                        , Html.text folder.name
                        , Html.text "\""
                        ]
        ]
