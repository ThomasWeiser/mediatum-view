module Article.Fts exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , update
    , view
    )

import Api
import Document exposing (Document, DocumentId)
import Folder exposing (Folder, FolderCounts)
import FtsDocumentResult exposing (FtsDocumentResult)
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Pagination.Offset.Page as Page exposing (Page, PageResult)
import Query exposing (Query)
import Utils


type alias Model =
    { query : Query
    , pageResult : PageResult FtsDocumentResult
    , queryFolderCounts : Bool
    }


type Msg
    = ApiResponseFtsPage (Api.Response (Page FtsDocumentResult))
    | ApiResponseFtsFolderCounts (Api.Response FolderCounts)
    | PickPosition Page.Position
    | SelectDocument DocumentId


type Return
    = NoReturn
    | SelectedDocument DocumentId
    | FolderCounts FolderCounts


init : Query -> ( Model, Cmd Msg )
init query =
    let
        model =
            { query = query
            , pageResult = Page.initialPageResult
            , queryFolderCounts = True
            }
    in
    update
        (PickPosition Page.First)
        model
        |> Utils.tupleRemoveThird


update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
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
                    model.query
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
                        model.query
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
            ( model, Cmd.none, SelectedDocument id )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] <|
            if model.query.searchString == "" then
                [ Html.span [] [ Html.text "All Documents" ] ]

            else
                [ Html.span [] [ Html.text "Search " ]
                , Html.span []
                    [ Html.text <|
                        Query.searchTypeToLabel model.query.searchType
                    ]
                , Html.span [] [ Html.text ": \"" ]
                , Html.span [] [ Html.text model.query.searchString ]
                , Html.span [] [ Html.text "\"" ]
                ]
        , case model.pageResult.page of
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
