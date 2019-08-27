module Article.Collection exposing
    ( Context
    , Model
    , Msg
    , initialModel
    , update
    , view
    )

import Api
import Data.Cache as Cache
import Data.Types exposing (FolderId)
import Folder
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Icons
import RemoteData


type alias Context =
    { cache : Cache.Model
    , folderId : FolderId
    }


type alias Model =
    ()


type Msg
    = NoOp


initialModel : Model
initialModel =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Context -> Model -> Html Msg
view context model =
    Html.div []
        [ case Cache.get context.cache.folders context.folderId of
            RemoteData.NotAsked ->
                -- Should never happen
                Icons.spinner

            RemoteData.Loading ->
                Icons.spinner

            RemoteData.Failure error ->
                viewApiError error

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


viewApiError : Api.Error -> Html msg
viewApiError error =
    viewError (Graphql.Extra.errorToString error)


viewError : String -> Html msg
viewError defect =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text defect ]
