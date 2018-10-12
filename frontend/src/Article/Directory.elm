module Article.Directory exposing
    ( Model
    , Msg
    , Specification
    , init
    , update
    , view
    )

import Api
import Document exposing (Document, DocumentId)
import Folder exposing (Folder)
import Graphqelm.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Pagination.Relay.Page as Page exposing (Page, PageResult)
import Pagination.Relay.Pagination as Pagination
import Utils


type alias Context =
    { folder : Folder
    }


type alias Model =
    { specification : Specification
    , pageResult : PageResult Document
    }


type alias Specification =
    ()


type Msg
    = ApiResponse (Api.Response (Page Document))
    | PickPosition Pagination.Position
    | SelectDocument DocumentId


init : Context -> Specification -> ( Model, Cmd Msg )
init context specification =
    let
        model =
            { specification = specification
            , pageResult = Page.initialPageResult
            }
    in
    update
        (PickPosition Pagination.First)
        context
        model
        |> Utils.tupleRemoveThird


update : Msg -> Context -> Model -> ( Model, Cmd Msg, Maybe DocumentId )
update msg context model =
    case msg of
        PickPosition position ->
            sendSearchQuery position context model
                |> Utils.tupleAddThird Nothing

        ApiResponse result ->
            ( { model
                | pageResult = Page.updatePageResultFromResult result model.pageResult
              }
            , Cmd.none
            , Nothing
            )

        SelectDocument id ->
            ( model, Cmd.none, Just id )


sendSearchQuery : Pagination.Position -> Context -> Model -> ( Model, Cmd Msg )
sendSearchQuery paginationPosition context model =
    ( { model
        | pageResult = Page.loadingPageResult model.pageResult
      }
    , Api.makeRequest
        ApiResponse
        (Api.queryFolderDocuments
            model.pageResult.page
            paginationPosition
            context.folder.id
        )
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] <|
            [ Html.span [] [ Html.text "All Documents" ] ]
        , case model.pageResult.page of
            Nothing ->
                Html.text ""

            Just documentPage ->
                viewResponse
                    PickPosition
                    (viewPage (Document.view SelectDocument Nothing))
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
    (Pagination.Position -> Msg)
    -> (Page itemModel -> Html Msg)
    -> Page itemModel
    -> Html Msg
viewResponse paginationTargetTagger viewEntity page =
    Html.div []
        [ viewNumberOfResults page
        , viewEntity page
        , viewPaginationButtons page paginationTargetTagger
        ]


viewPage : (itemModel -> Html Msg) -> Page itemModel -> Html Msg
viewPage viewItem page =
    Html.div []
        [ Html.div []
            (List.map viewItem (Page.entries page))
        ]


viewNumberOfResults : Page itemModel -> Html msg
viewNumberOfResults page =
    Html.div []
        [ Html.strong
            []
            [ Html.text "Number of Results: "
            , Html.text <| toString page.totalCount
            ]
        ]


viewPaginationButtons : Page itemModel -> (Pagination.Position -> Msg) -> Html Msg
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
        [ -- "Last" currently does not work with our GraphQL API
          -- Only show "First" and "Next" for now.
          -- May implement a form of infinite scrolling later.
          viewButton "First"
            (targetTagger Pagination.First)
            page.pageInfo.hasPreviousPage

        {-
           , viewButton "Prev"
               (targetTagger Pagination.Previous)
               page.pageInfo.hasPreviousPage
        -}
        , viewButton "Next"
            (targetTagger Pagination.Next)
            page.pageInfo.hasNextPage

        {-
           , viewButton "Last"
               (targetTagger Pagination.Last)
               page.pageInfo.hasNextPage
        -}
        ]


viewError : Graphqelm.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (toString error) ]
