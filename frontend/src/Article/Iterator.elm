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
import FtsDocumentResult exposing (FtsDocumentResult)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Utils


type alias Context item =
    { folder : Folder
    , itemList : List item
    , itemId : item -> DocumentId
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


view : Context item -> Model -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "iterator" ]
        [ Html.button
            [ Html.Events.onClick Show ]
            [ Html.text "Show" ]
        , Html.button
            [ Html.Events.onClick Close ]
            [ Html.text "All Results" ]
        , Html.text <|
            Debug.toString <|
                Utils.findAdjacent
                    context.itemList
                    (\item -> context.itemId item == model.currentId)
        , Details.view model.details
            |> Html.map DetailsMsg
        ]
