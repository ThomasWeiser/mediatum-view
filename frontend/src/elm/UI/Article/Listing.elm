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

import Cache exposing (Cache)
import Constants
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
import Types.ApiData exposing (ApiData)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.Id exposing (DocumentId)
import Types.Localization as Localization
import Types.Navigation as Navigation exposing (Navigation)
import Types.Route exposing (Route)
import Types.Selection exposing (Selection)
import UI.Icons
import Utils.Html


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , route : Route
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
    | LoadMore



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
        LoadMore ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.SetLimit
                    (Constants.incrementLimitOnLoadMore context.limit)
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
    let
        pageSequence =
            Cache.getDocumentsPages
                context.cache
                ( Config.getMaskName MasksConfig.MaskForListing context.config
                , context.selection
                )
    in
    Html.div [] <|
        -- case model.iterator of
        -- Nothing ->
        [ viewPageSequence context pageSequence
        , viewFooter context pageSequence
        ]


{-| -}
viewPageSequence : Context -> PageSequence -> Html Msg
viewPageSequence context pageSequence =
    Html.div []
        (PageSequence.presentationSegmentsLimited context.limit pageSequence
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
                viewSpinner

            RemoteData.Loading ->
                viewSpinner

            RemoteData.Failure error ->
                Utils.Html.viewApiError error

            RemoteData.Success documentsPage ->
                viewDocumentsPage context documentsPage
        ]


viewSpinner : Html msg
viewSpinner =
    Html.div
        [ Html.Attributes.class "text-align-center" ]
        [ UI.Icons.spinner ]


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
    Html.a
        [ Html.Attributes.class "document"
        , Navigation.alterRouteHref
            context
            (Navigation.ShowDocument context.selection.scope document.id)
        ]
        [ Html.div [ Html.Attributes.class "metadatatype" ]
            [ Html.span [ Html.Attributes.class "result-number" ]
                [ Html.text <| String.fromInt number ++ ". "
                , Html.text document.metadatatypeName
                ]
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


keys :
    { author : Regex.Regex
    , congressOrJournal : Regex.Regex
    , title : Regex.Regex
    , titleOrType : Regex.Regex
    , year : Regex.Regex
    }
keys =
    let
        regex regexString =
            Maybe.withDefault Regex.never (Regex.fromString regexString)
    in
    { author = regex "author"
    , title = regex "title"
    , congressOrJournal = regex "congress|journal"
    , year = regex "year"
    , titleOrType = regex "title|type"
    }


viewAttribute : Document.Attribute -> Html msg
viewAttribute attribute =
    let
        isField regex =
            Regex.contains regex attribute.field
    in
    case attribute.value of
        Just value ->
            if Entities.Markup.isEmpty value then
                Html.text ""

            else
                Html.span
                    [ Html.Attributes.classList
                        [ ( "attribute", True )
                        , ( "author", isField keys.author )
                        , ( "title"
                          , isField keys.title
                                && not (isField keys.congressOrJournal)
                          )
                        ]
                    ]
                    (let
                        markup =
                            value
                                |> Entities.Markup.trim Constants.maxAttributeLengthInListingView
                                |> Entities.Markup.view
                     in
                     if isField keys.year then
                        [ value |> Entities.Markup.normalizeYear |> Entities.Markup.view
                        , Html.text ". "
                        ]

                     else if isField keys.author then
                        [ markup
                        , Html.text ": "
                        ]

                     else if isField keys.titleOrType then
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


viewFooter : Context -> PageSequence -> Html Msg
viewFooter context pageSequence =
    let
        viewButton : Localization.Translations -> Msg -> Bool -> Html Msg
        viewButton label msg enabled =
            Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.disabled (not enabled)
                , Html.Events.onClick msg
                ]
                [ Localization.text context.config label ]

        canLoadMore =
            PageSequence.canLoadMore context.limit pageSequence
    in
    if canLoadMore then
        if context.limit < context.config.maxLimit then
            Html.div
                [ Html.Attributes.style "margin" "4px 0px 8px 0px"
                , Html.Attributes.class "input-group"
                ]
                [ viewButton
                    { en = "More Results"
                    , de = "weitere Ergebnisse"
                    }
                    LoadMore
                    (PageSequence.remoteDataIsSuccess pageSequence)
                ]

        else
            Html.div
                [ Html.Attributes.style "margin" "4px 0px 8px 0px"
                , Html.Attributes.class "no-more-results"
                ]
                [ Html.hr [] []
                , Localization.text context.config
                    { en = "Maximum Number of Results Reached"
                    , de = "maximale Anzahl von Ergebnissen erreicht"
                    }
                ]

    else
        Html.div
            [ Html.Attributes.style "margin" "4px 0px 8px 0px"
            , Html.Attributes.class "no-more-results"
            ]
            [ Html.hr [] []
            , Localization.text context.config
                { en = "No More Results"
                , de = "keine weiteren Ergebnisse"
                }
            ]
