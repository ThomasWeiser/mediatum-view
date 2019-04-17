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
import Maybe.Extra
import Query.Filters
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
    | Select DocumentId


init : Context item -> DocumentId -> ( Model, Cmd Msg )
init context documentId =
    let
        ( subModel, subCmd ) =
            Details.init
                { detailsQuery =
                    { folder = context.folder
                    , documentId = documentId
                    , filters = Query.Filters.none
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

        Select documentId ->
            init context documentId
                |> Utils.tupleAddThird NoReturn


view : Context item -> Model -> Html Msg
view context model =
    let
        first =
            List.head context.itemList
                |> Maybe.map context.itemId
                |> Maybe.Extra.filter ((/=) model.currentId)

        prev =
            case adjacent of
                Just ( Just prevItem, _, _ ) ->
                    Just (context.itemId prevItem)

                _ ->
                    Nothing

        next =
            case adjacent of
                Just ( _, _, Just nextItem ) ->
                    Just (context.itemId nextItem)

                _ ->
                    Nothing

        adjacent =
            Utils.findAdjacent
                context.itemList
                (\item -> context.itemId item == model.currentId)

        selectButtonAttrs maybeId =
            Maybe.Extra.unwrap
                [ Html.Attributes.type_ "button"
                , Html.Attributes.disabled True
                ]
                (\id -> [ Html.Events.onClick (Select id) ])
                maybeId
    in
    Html.div
        [ Html.Attributes.class "iterator" ]
        [ Html.div
            [ Html.Attributes.class "button-group" ]
            [ Html.button
                [ Html.Attributes.type_ "button"
                , Html.Events.onClick Show
                ]
                [ Html.text "Show" ]
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Events.onClick Close
                ]
                [ Html.text "All Results" ]
            , Html.button
                (selectButtonAttrs first)
                [ Html.text "First" ]
            , Html.button
                (selectButtonAttrs prev)
                [ Html.text "Prev" ]
            , Html.button
                (selectButtonAttrs next)
                [ Html.text "Next" ]
            ]
        , Details.view model.details
            |> Html.map DetailsMsg
        ]
