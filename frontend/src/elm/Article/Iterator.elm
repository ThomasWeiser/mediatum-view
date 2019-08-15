module Article.Iterator exposing
    ( Context
    , Model
    , Msg
    , Return(..)
    , initialModel
    , update
    , view
    )

import Article.Details as Details
import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (Document, DocumentId, Folder)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import Query.Filters
import Utils


type alias Context item =
    { cache : Cache.Model
    , folder : Folder
    , itemList : List item
    , itemId : item -> DocumentId
    }


type Return
    = NoReturn
    | ShowDocument DocumentId
    | CloseIterator
    | UpdateCacheWithModifiedDocument Document


type alias Model =
    { currentId : DocumentId
    , details : Details.Model
    }


type Msg
    = DetailsMsg Details.Msg
    | Show
    | Close
    | Select DocumentId


initialModel : Context item -> DocumentId -> Model
initialModel context documentId =
    let
        subModel =
            Details.initialModel
                { cache = context.cache
                , detailsQuery =
                    { folder = context.folder
                    , documentId = documentId
                    , filters = Query.Filters.none
                    }
                }
    in
    { currentId = documentId
    , details = subModel
    }


update : Context item -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        DetailsMsg subMsg ->
            let
                ( subModel, subCmd, subReturn ) =
                    Details.update
                        { cache = context.cache
                        , detailsQuery =
                            { folder = context.folder
                            , documentId = model.currentId
                            , filters = Query.Filters.none
                            }
                        }
                        subMsg
                        model.details
            in
            ( { model | details = subModel }
            , Cmd.map DetailsMsg subCmd
            , case subReturn of
                Details.NoReturn ->
                    NoReturn

                Details.UpdateCacheWithModifiedDocument document ->
                    UpdateCacheWithModifiedDocument document
            )

        Close ->
            ( model, Cmd.none, CloseIterator )

        Show ->
            ( model, Cmd.none, ShowDocument model.currentId )

        Select documentId ->
            ( initialModel context documentId
            , Cmd.none
            , NoReturn
            )


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
                (\item -> context.itemId item == model.currentId)
                context.itemList

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
            [ Html.Attributes.class "input-group" ]
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
        , Details.view
            { cache = context.cache
            , detailsQuery =
                { folder = context.folder
                , documentId = model.currentId
                , filters = Query.Filters.none
                }
            }
            model.details
            |> Html.map DetailsMsg
        ]
