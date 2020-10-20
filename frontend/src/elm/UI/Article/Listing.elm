module UI.Article.Listing exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs initialModel
@docs update
@docs view

-}

-- import Article.Iterator as Iterator
-- import Pagination.Offset.Page as Page exposing (Page, PageResult)

import Basics.Extra
import Cache exposing (Cache)
import Entities.Document as Document exposing (Document)
import Entities.DocumentResults exposing (DocumentResult, DocumentsPage)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Regex
import RemoteData
import Types exposing (Window)
import Types.Id exposing (DocumentId)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Route as Route
import Types.Route.Url
import Types.Selection exposing (Selection)
import UI.Icons
import Utils.Html
import Utils.Markup


{-| -}
type alias Context =
    { cache : Cache
    , selection : Selection
    , window : Window
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation


{-| -}
type alias Model =
    { -- , iterator : Maybe Iterator.Model
    }


{-| -}
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


{-| -}
initialModel : Model
initialModel =
    { -- , iterator = Nothing
    }


{-| -}
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
                            (context.window.offset - context.window.limit)
                                |> Basics.Extra.atLeast 0

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


{-| -}
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
                UI.Icons.spinner

            RemoteData.Loading ->
                UI.Icons.spinner

            RemoteData.Failure error ->
                Utils.Html.viewApiError error

            RemoteData.Success documentsPage ->
                viewDocumentsPage context documentsPage
        ]


viewDocumentsPage : Context -> DocumentsPage -> Html Msg
viewDocumentsPage context documentsPage =
    Html.div []
        [ -- viewNumberOfResults page,
          Html.div
            [ Html.Attributes.class "listing" ]
            (List.map
                (viewDocumentResult context)
                documentsPage.content
            )
        , viewPaginationButtons documentsPage
        ]


viewDocumentResult : Context -> DocumentResult -> Html Msg
viewDocumentResult context documentResult =
    viewDocument
        context
        documentResult.number
        documentResult.document


viewDocument : Context -> Int -> Document -> Html Msg
viewDocument context number document =
    Html.div [ Html.Attributes.class "document" ]
        [ Html.div [ Html.Attributes.class "metadatatype" ]
            [ Html.span [ Html.Attributes.class "result-number" ]
                [ Html.text <| String.fromInt number ++ ". " ]
            , Html.a
                [ Html.Attributes.class "metadatatype"
                , Route.initDocumentInFolder
                    context.selection.scope
                    document.id
                    |> Types.Route.Url.toString
                    |> Html.Attributes.href
                ]
                [ Html.text document.metadatatypeName ]
            ]
        , Html.div
            [ Html.Attributes.class "attributes"
            , Html.Events.onClick (SelectDocument document.id)
            ]
            (List.map
                viewAttribute
                document.attributes
            )
        ]


maxAttributeStringLength : Int
maxAttributeStringLength =
    80


viewAttribute : Document.Attribute -> Html msg
viewAttribute attribute =
    let
        isField regexString =
            Regex.contains
                (Maybe.withDefault Regex.never (Regex.fromString regexString))
                attribute.field
    in
    case attribute.value of
        Just value ->
            Html.span
                [ Html.Attributes.classList
                    [ ( "attribute", True )
                    , ( "author", isField "author" )
                    , ( "title"
                      , isField "title"
                            && not (isField "congress|journal")
                      )
                    ]
                ]
                (let
                    markup =
                        value |> Utils.Markup.parse |> Utils.Markup.view
                 in
                 if isField "year" then
                    [ value |> Utils.Markup.parse |> Utils.Markup.normalizeYear |> Utils.Markup.view
                    , Html.text ". "
                    ]

                 else if isField "author" then
                    [ markup
                    , Html.text ": "
                    ]

                 else if isField "title|type" then
                    [ markup
                    , Html.text ". "
                    ]

                 else
                    [ Html.text (attribute.name ++ ": ")
                    , markup
                    , Html.text ". "
                    ]
                )

        Nothing ->
            Html.text ""


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
