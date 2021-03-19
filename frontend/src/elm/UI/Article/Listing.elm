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
import Entities.DocumentResults exposing (DocumentResult)
import Entities.Markup
import Entities.PageSequence as PageSequence exposing (PageSequence)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import Regex
import RemoteData
import Types
import Types.ApiData exposing (ApiData)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.Id exposing (DocumentId)
import Types.Localization as Localization
import Types.Navigation as Navigation exposing (Navigation)
import Types.Route as Route
import Types.Route.Url
import Types.Selection exposing (Selection)
import UI.Icons
import Utils.Html


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , selection : Selection
    , limit : Int
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
    | ShiftLimit (Int -> Int)



-- | IteratorMsg Iterator.Msg
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
        ShiftLimit mapping ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.SetLimit
                    (mapping context.limit |> Basics.Extra.atLeast 0)
                )
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
        [ viewFooter context
        , viewPageSequence context
            (Cache.getDocumentsPages
                context.cache
                ( Config.getMaskName MasksConfig.MaskForListing context.config
                , context.selection
                )
            )
        , viewFooter context
        ]


{-| -}
viewPageSequence : Context -> PageSequence -> Html Msg
viewPageSequence context pageSequence =
    Html.div []
        (PageSequence.presentationSegments context.limit pageSequence
            |> List.map
                (viewPageApiData context)
            |> List.intersperse (Html.hr [] [])
        )


{-| -}
viewPageApiData : Context -> ApiData (List DocumentResult) -> Html Msg
viewPageApiData context apiData =
    Html.div [] <|
        [ case apiData of
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


viewDocumentsPage : Context -> List DocumentResult -> Html Msg
viewDocumentsPage context documentResults =
    Html.div []
        [ Html.div
            [ Html.Attributes.class "listing" ]
            (List.map
                (viewDocumentResult context)
                documentResults
            )
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
                    context.config
                    context.selection.scope
                    document.id
                    |> Types.Route.Url.toString context.config
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
        , viewSearchMatching context.config document.searchMatching
        ]


viewSearchMatching : Config -> Maybe Document.SearchMatching -> Html msg
viewSearchMatching config =
    Maybe.Extra.unwrap
        { en = "", de = "" }
        (\{ attributes, fulltext } ->
            case ( attributes, fulltext ) of
                ( False, False ) ->
                    { en = "", de = "" }

                ( True, False ) ->
                    { en = "Search term found in metadata"
                    , de = "Suchbegriff in Metadaten gefunden"
                    }

                ( False, True ) ->
                    { en = "Search term found in fulltext"
                    , de = "Suchbegriff in Volltext gefunden"
                    }

                ( True, True ) ->
                    { en = "Search term found in metadata and fulltext"
                    , de = "Suchbegriff in Metadaten und Volltext gefunden"
                    }
        )
        >> Localization.text config


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
                        Entities.Markup.view value
                 in
                 if isField "year" then
                    [ value |> Entities.Markup.normalizeYear |> Entities.Markup.view
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


viewFooter : Context -> Html Msg
viewFooter context =
    let
        viewButton : Localization.Translations -> Msg -> Bool -> Html Msg
        viewButton label msg enabled =
            Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.disabled (not enabled)
                , Html.Events.onClick msg
                ]
                [ Localization.text context.config label ]

        viewButtonTest : String -> Msg -> Html Msg
        viewButtonTest text msg =
            viewButton { en = text, de = text } msg True
    in
    Html.div
        [ Html.Attributes.style "margin" "4px 0px 8px 0px"
        , Html.Attributes.class "input-group"
        ]
        [ {- viewButton { en = "More", de = "mehr" }
             (PickPosition Next)
             True
          -}
          viewButtonTest "Limit = 0" (ShiftLimit (always 0))
        , viewButtonTest "Limit + 1" (ShiftLimit ((+) 1))
        , viewButtonTest "Limit + 4" (ShiftLimit ((+) 4))
        , viewButtonTest "Limit - 1" (ShiftLimit ((+) -1))
        , viewButtonTest "Limit - 4" (ShiftLimit ((+) -4))
        ]
