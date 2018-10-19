module Article.Iterator exposing
    ( Context
    , Model
    , Msg
    , Return(..)
    , init
    , update
    , view
    )

import Article.Details as Details
import Document exposing (DocumentId)
import Folder exposing (Folder)
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Context item =
    { folder : Folder
    , idList : List item
    }


type Return
    = NoReturn
    | ShowDocument DocumentId
    | CloseIterator


type alias Model =
    { currentId : DocumentId
    , details : Details.Model
    }


type Msg
    = DetailsMsg Details.Msg
    | Show
    | Close


init : Context item -> DocumentId -> ( Model, Cmd Msg )
init context documentId =
    let
        ( subModel, subCmd ) =
            Details.init
                { detailsQuery =
                    { folder = context.folder
                    , documentId = documentId
                    }
                }
    in
    ( { currentId = documentId
      , details = subModel
      }
    , Cmd.map DetailsMsg subCmd
    )


update : Context item -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        DetailsMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Details.update subMsg model.details
            in
            ( { model | details = subModel }
            , Cmd.map DetailsMsg subCmd
            , NoReturn
            )

        Close ->
            ( model, Cmd.none, CloseIterator )

        Show ->
            ( model, Cmd.none, ShowDocument model.currentId )


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "iterator" ]
        [ Html.button
            [ Html.Events.onClick Show ]
            [ Html.text "Show" ]
        , Html.button
            [ Html.Events.onClick Close ]
            [ Html.text "Close" ]
        , Details.view model.details
            |> Html.map DetailsMsg
        ]
