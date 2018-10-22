module Article.Fts exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , update
    , view
    )

import Api
import Article.Iterator as Iterator
import Document exposing (Document, DocumentId)
import Folder exposing (Folder, FolderCounts)
import FtsDocumentResult exposing (FtsDocumentResult)
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Pagination.Offset.Page as Page exposing (Page, PageResult)
import Query
import Utils


type alias Context =
    { ftsQuery : Query.Fts
    }


type Return
    = NoReturn
    | ShowDocument DocumentId
    | FolderCounts FolderCounts


type alias Model =
    { pageResult : PageResult FtsDocumentResult
    , queryFolderCounts : Bool
    , iterator : Maybe Iterator.Model
    }


type Msg
    = ApiResponseFtsPage (Api.Response (Page FtsDocumentResult))
    | ApiResponseFtsFolderCounts (Api.Response FolderCounts)
    | PickPosition Page.Position
    | SelectDocument DocumentId
    | IteratorMsg Iterator.Msg


init : Context -> ( Model, Cmd Msg )
init context =
    let
        model =
            { pageResult = Page.initialPageResult
            , queryFolderCounts = True
            , iterator = Nothing
            }
    in
    update
        context
        (PickPosition Page.First)
        model
        |> Utils.tupleRemoveThird


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        PickPosition paginationPosition ->
            ( { model
                | pageResult = Page.loadingPageResult model.pageResult
              }
            , Api.makeRequest
                ApiResponseFtsPage
                (Api.queryFtsPage
                    model.pageResult.page
                    paginationPosition
                    context.ftsQuery
                )
            , NoReturn
            )

        ApiResponseFtsPage result ->
            ( { model
                | pageResult = Page.updatePageResultFromResult result model.pageResult
                , queryFolderCounts = False
              }
            , if model.queryFolderCounts then
                Api.makeRequest
                    ApiResponseFtsFolderCounts
                    (Api.queryFtsFolderCounts
                        context.ftsQuery
                    )

              else
                Cmd.none
            , NoReturn
            )

        ApiResponseFtsFolderCounts (Err error) ->
            -- TODO
            ( model
            , Cmd.none
            , NoReturn
            )

        ApiResponseFtsFolderCounts (Ok folderCountMap) ->
            ( model
            , Cmd.none
            , FolderCounts folderCountMap
            )

        SelectDocument id ->
            let
                ( subModel, subCmd ) =
                    Iterator.init
                        { folder = context.ftsQuery.folder
                        , idList = [] -- TODO
                        }
                        id
            in
            ( { model | iterator = Just subModel }
            , subCmd |> Cmd.map IteratorMsg
            , NoReturn
            )

        IteratorMsg subMsg ->
            case model.iterator of
                Nothing ->
                    ( model, Cmd.none, NoReturn )

                Just iterator ->
                    let
                        ( subModel, subCmd, subReturn ) =
                            Iterator.update
                                { folder = context.ftsQuery.folder
                                , idList = [] -- TODO
                                }
                                subMsg
                                iterator
                    in
                    ( { model
                        | iterator =
                            if subReturn == Iterator.CloseIterator then
                                Nothing

                            else
                                Just subModel
                      }
                    , Cmd.map IteratorMsg subCmd
                    , case subReturn of
                        Iterator.ShowDocument id ->
                            ShowDocument id

                        _ ->
                            NoReturn
                    )


view : Model -> Html Msg
view model =
    Html.div [] <|
        case model.iterator of
            Nothing ->
                [ case model.pageResult.page of
                    Nothing ->
                        Html.text ""

                    Just documentPage ->
                        viewResponse
                            PickPosition
                            (viewPage (FtsDocumentResult.view SelectDocument))
                            documentPage
                , if model.pageResult.loading then
                    Icons.spinner

                  else
                    Html.text ""
                , case model.pageResult.error of
                    Nothing ->
                        Html.text ""

                    Just error ->
                        viewError error
                , case model.iterator of
                    Nothing ->
                        Html.text ""

                    Just iterator ->
                        Iterator.view iterator |> Html.map IteratorMsg
                ]

            Just iterator ->
                [ Iterator.view iterator |> Html.map IteratorMsg
                ]


viewResponse :
    (Page.Position -> Msg)
    -> (Page itemModel -> Html Msg)
    -> Page itemModel
    -> Html Msg
viewResponse paginationTargetTagger viewEntity page =
    Html.div []
        [ -- viewNumberOfResults page,
          viewEntity page
        , viewPaginationButtons page paginationTargetTagger
        ]


viewPage : (itemModel -> Html Msg) -> Page itemModel -> Html Msg
viewPage viewItem page =
    Html.div []
        [ Html.div []
            (List.map viewItem (Page.entries page))
        ]


viewPaginationButtons : Page itemModel -> (Page.Position -> Msg) -> Html Msg
viewPaginationButtons page targetTagger =
    let
        viewButton : String -> Msg -> Bool -> Html Msg
        viewButton label msg enabled =
            Html.button
                [ Html.Attributes.disabled (not enabled)
                , Html.Events.onClick msg
                ]
                [ Html.text label ]
    in
    Html.div
        [ Html.Attributes.style "margin" "4px 0px 8px 0px"
        ]
        [ viewButton "First"
            (targetTagger Page.First)
            (not (Page.isFirstPage page))
        , viewButton "Prev"
            (targetTagger Page.Previous)
            (not (Page.isFirstPage page))
        , viewButton "Next"
            (targetTagger Page.Next)
            page.hasNextPage
        ]


viewError : Graphql.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (Graphql.Extra.errorToString error) ]
