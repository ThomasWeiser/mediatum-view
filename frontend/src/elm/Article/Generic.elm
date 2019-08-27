module Article.Generic exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (NodeId)
import Html exposing (Html)


type alias Context =
    { cache : Cache.Model
    , nodeIds : Maybe ( NodeId, Maybe NodeId )
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
        [ Html.text "Article.Generic: "
        , Html.text <| Debug.toString context.nodeIds
        ]
