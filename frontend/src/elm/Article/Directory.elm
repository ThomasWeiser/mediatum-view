module Article.Directory exposing
    ( Model
    , Msg
    , Return(..)
    , init
    , update
    , view
    )

-- import Article.Iterator as Iterator

import Api
import Api.Queries
import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (DocumentId, DocumentResult, FolderCounts)
import DocumentResult
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Maybe.Extra
import Pagination.Offset.Page as Page exposing (Page, PageResult)
import Query
import Utils


type alias Context =
    { cache : Cache.Model
    , folderQuery : Query.FolderQuery
    }


type Return
    = NoReturn
    | ShowDocument DocumentId
    | FolderCounts FolderCounts


type alias Model =
    { pageResult : PageResult DocumentResult

    -- , iterator : Maybe Iterator.Model
    , doQueryFolderCounts : Bool
    }


type Msg
    = ApiResponsePage (Api.Response (Page DocumentResult))
    | ApiResponseFolderCounts (Api.Response FolderCounts)
    | PickPosition Page.Position
    | SelectDocument DocumentId



-- | IteratorMsg Iterator.Msg
{-
   iteratorContext : Context -> Model -> Iterator.Context DocumentResult
   iteratorContext context model =
       { cache = context.cache
       , folder = context.folderQuery.folder
       , itemList = Maybe.Extra.unwrap [] Page.entries model.pageResult.page
       , itemId = .document >> .id
       }
-}


init : Context -> ( Model, Cmd Msg )
init context =
    let
        model =
            { pageResult = Page.initialPageResult

            -- , iterator = Nothing
            , doQueryFolderCounts = True
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
            , Api.sendQueryRequest
                ApiResponsePage
                (Api.Queries.folderDocumentsPage_ByQuery
                    model.pageResult.page
                    paginationPosition
                    context.folderQuery
                )
            , NoReturn
            )

        ApiResponsePage result ->
            ( { model
                | pageResult = Page.updatePageResultFromResult result model.pageResult
                , doQueryFolderCounts = False
              }
            , if model.doQueryFolderCounts then
                Api.sendQueryRequest
                    ApiResponseFolderCounts
                    (Api.Queries.folderDocumentsFolderCounts_ByQuery
                        context.folderQuery
                    )

              else
                Cmd.none
            , NoReturn
            )

        ApiResponseFolderCounts (Err error) ->
            -- TODO
            ( model
            , Cmd.none
            , NoReturn
            )

        ApiResponseFolderCounts (Ok folderCountMap) ->
            ( model
            , Cmd.none
            , FolderCounts folderCountMap
            )

        SelectDocument id ->
            ( model
              {- { model
                   | iterator =
                       Just
                           (Iterator.initialModel
                               (iteratorContext context model)
                               id
                           )
                 }
              -}
            , Cmd.none
            , NoReturn
            )



{- IteratorMsg subMsg ->
   case model.iterator of
       Nothing ->
           ( model, Cmd.none, NoReturn )

       Just iterator ->
           let
               ( subModel, subCmd, subReturn ) =
                   Iterator.update
                       (iteratorContext context model)
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
-}


view : Context -> Model -> Html Msg
view context model =
    Html.div [] <|
        -- case model.iterator of
        -- Nothing ->
        [ case model.pageResult.page of
            Nothing ->
                Html.text ""

            Just documentPage ->
                viewResponse
                    PickPosition
                    (viewPage (DocumentResult.view SelectDocument))
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



{-
   Just iterator ->
       [ Iterator.view
           (iteratorContext context model)
           iterator
           |> Html.map IteratorMsg
       ]
-}


viewResponse :
    (Page.Position -> Msg)
    -> (Page itemModel -> Html Msg)
    -> Page itemModel
    -> Html Msg
viewResponse paginationTargetTagger viewEntity page =
    Html.div []
        [ viewEntity page
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
                [ Html.Attributes.type_ "button"
                , Html.Attributes.disabled (not enabled)
                , Html.Events.onClick msg
                ]
                [ Html.text label ]
    in
    Html.div
        [ Html.Attributes.style "margin" "4px 0px 8px 0px"
        , Html.Attributes.class "input-group"
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


viewError : Api.Error -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (Graphql.Extra.errorToString error) ]
