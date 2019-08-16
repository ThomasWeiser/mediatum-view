module Article.Directory exposing
    ( Model
    , Msg
    , Return(..)
    , initialModel
    , update
    , view
    )

-- import Article.Iterator as Iterator
-- import Pagination.Offset.Page as Page exposing (Page, PageResult)

import Api
import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (DocumentId, DocumentResult, DocumentsPage)
import DocumentResult
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Maybe.Extra
import Query
import RemoteData
import Utils


type alias Context =
    { cache : Cache.Model
    , folderQuery : Query.FolderQuery
    }


type Return
    = NoReturn
    | ShowDocument DocumentId


type alias Model =
    { -- , iterator : Maybe Iterator.Model
    }


type Msg
    = SelectDocument DocumentId



-- | PickPosition Page.Position
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


initialModel : Context -> Model
initialModel context =
    { -- , iterator = Nothing
    }


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        {-
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
        -}
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
    let
        selection =
            { scope = context.folderQuery.folder.id
            , searchMethod = Data.Types.SelectByFolderListing
            , filters = context.folderQuery.filters
            }

        window =
            { offset = 0, limit = 10 }
    in
    Html.div [] <|
        -- case model.iterator of
        -- Nothing ->
        [ case
            Cache.get
                context.cache.documentsPages
                ( selection, window )
          of
            RemoteData.NotAsked ->
                -- Should never happen
                Icons.spinner

            RemoteData.Loading ->
                Icons.spinner

            RemoteData.Failure error ->
                viewApiError error

            RemoteData.Success documentsPage ->
                viewDocumentsPage documentsPage
        ]



{-
   Just iterator ->
       [ Iterator.view
           (iteratorContext context model)
           iterator
           |> Html.map IteratorMsg
       ]
-}


viewDocumentsPage : DocumentsPage -> Html Msg
viewDocumentsPage documentsPage =
    Html.div []
        [ Html.div []
            (List.map
                (DocumentResult.view SelectDocument)
                documentsPage.content
            )

        -- , viewPaginationButtons page paginationTargetTagger
        ]



{-
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
-}


viewApiError : Api.Error -> Html msg
viewApiError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (Graphql.Extra.errorToString error) ]
