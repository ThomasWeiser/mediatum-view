module Article.DocumentsPage exposing
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
import Data.Types exposing (..)
import DocumentResult
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Maybe.Extra
import Navigation exposing (Navigation)
import RemoteData
import Route
import Utils


type alias Context =
    { cache : Cache.Model
    , selection : Selection
    , window : Window
    }


type Return
    = NoReturn
    | Navigate Navigation


type alias Model =
    { -- , iterator : Maybe Iterator.Model
    }


type Msg
    = SelectDocument DocumentId
    | PickPosition PaginationPosition



-- | IteratorMsg Iterator.Msg


type PaginationPosition
    = First
    | Previous
    | Next



{-
   iteratorContext : Context -> Model -> Iterator.Context DocumentResult
   iteratorContext context model =
       { cache = context.cache
       , folder = context.ftsQuery.folder
       , itemList = Maybe.Extra.unwrap [] Page.entries model.pageResult.page
       , itemId = .document >> .id
       }
-}


initialModel : Model
initialModel =
    { -- , iterator = Nothing
    }


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        PickPosition position ->
            let
                newOffset =
                    case position of
                        First ->
                            0

                        Previous ->
                            context.window.offset - context.window.limit |> Basics.max 0

                        Next ->
                            context.window.offset + context.window.limit
            in
            ( model
            , Cmd.none
            , Navigate (Navigation.SetOffset newOffset)
            )

        SelectDocument documentId ->
            ( model
              {- { model
                   | iterator =
                       Just
                           (Iterator.initialModel
                               (iteratorContext context model)
                               documentId
                           )
                 }
              -}
            , Cmd.none
            , Navigate
                (Navigation.ShowDocument context.selection.scope documentId)
            )



{-
   IteratorMsg subMsg ->
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
        [ case
            Cache.get
                context.cache.documentsPages
                ( context.selection, context.window )
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
        [ -- viewNumberOfResults page,
          Html.div []
            (List.map
                (DocumentResult.view SelectDocument)
                documentsPage.content
            )
        , viewPaginationButtons documentsPage
        ]


viewPaginationButtons : DocumentsPage -> Html Msg
viewPaginationButtons documentsPage =
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
            (PickPosition First)
            (documentsPage.offset /= 0)
        , viewButton "Prev"
            (PickPosition Previous)
            (documentsPage.offset /= 0)
        , viewButton "Next"
            (PickPosition Next)
            documentsPage.hasNextPage
        ]


viewApiError : Api.Error -> Html msg
viewApiError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (Graphql.Extra.errorToString error) ]
