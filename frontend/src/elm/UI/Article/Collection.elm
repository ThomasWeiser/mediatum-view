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
import RemoteData
import Types.Id exposing (FolderId)
import UI.Icons
import Utils.Html


{-| -}
type alias Context =
    { cache : Cache
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


{-| -}
view : Context -> Model -> Html Msg
view context model =
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
                        [ Html.text "Front page for root of all collections" ]

                    else
                        [ Html.text "Front page for collection \""
                        , Html.text folder.name
                        , Html.text "\""
                        ]
        ]
