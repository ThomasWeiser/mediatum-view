module Article.Directory exposing
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
import Folder exposing (Folder)
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Pagination.Relay.Page as Page exposing (Page, PageResult)
import Pagination.Relay.Pagination as Pagination
import Query
import Utils


type alias Context =
    { directoryQuery : Query.Directory
    }


type Return
    = NoReturn {- | FolderCounts FolderCounts -}
    | ShowDocument DocumentId


type alias Model =
    { pageResult : PageResult Document
    , iterator : Maybe Iterator.Model
    }


type Msg
    = ApiResponse (Api.Response (Page Document))
    | PickPosition Pagination.Position
    | SelectDocument DocumentId
    | IteratorMsg Iterator.Msg


init : Context -> ( Model, Cmd Msg )
init context =
    let
        model =
            { pageResult = Page.initialPageResult
            , iterator = Nothing
            }
    in
    update
        context
        (PickPosition Pagination.First)
        model
        |> Utils.tupleRemoveThird


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        PickPosition position ->
            sendSearchQuery context position model
                |> Utils.tupleAddThird NoReturn

        ApiResponse result ->
            ( { model
                | pageResult = Page.updatePageResultFromResult result model.pageResult
              }
            , Cmd.none
            , NoReturn
            )

        SelectDocument id ->
            let
                ( subModel, subCmd ) =
                    Iterator.init
                        { folder = context.directoryQuery.folder
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
                                { folder = context.directoryQuery.folder
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


sendSearchQuery : Context -> Pagination.Position -> Model -> ( Model, Cmd Msg )
sendSearchQuery context paginationPosition model =
    ( { model
        | pageResult = Page.loadingPageResult model.pageResult
      }
    , Api.makeRequest
        ApiResponse
        (Api.queryFolderDocuments
            model.pageResult.page
            paginationPosition
            context.directoryQuery
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
        , case model.iterator of
            Nothing ->
                Html.text ""

            Just iterator ->
                Iterator.view iterator |> Html.map IteratorMsg
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
            , Html.text <| String.fromInt page.totalCount
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


viewError : Graphql.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (Graphql.Extra.errorToString error) ]
