module UI.Article.Listing exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , update
    , hasAtLeastOneDocument
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs initialModel
@docs update
@docs hasAtLeastOneDocument
@docs view

-}

import Cache exposing (Cache)
import Constants
import Entities.Document as Document exposing (Document)
import Entities.DocumentResults exposing (DocumentResult)
import Entities.Markup
import Entities.PageSequence as PageSequence exposing (PageSequence)
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
    {}


{-| -}
type Msg
    = SelectDocument DocumentId
    | LoadMore


{-| -}
initialModel : Model
initialModel =
    {}


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        LoadMore ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.SetLimit
                    (Constants.incrementLimitOnLoadMore context.config context.limit)
                )
            )

        SelectDocument documentId ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.ShowDocument context.selection.scope documentId)
            )


{-| -}
hasAtLeastOneDocument : Context -> Bool
hasAtLeastOneDocument context =
    let
        pageSequence =
            Cache.getDocumentsPages
                context.cache
                ( Config.getMaskName MasksConfig.MaskForListing context.config
                , context.selection
                )
    in
    PageSequence.hasAtLeastOneDocument pageSequence


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
        [ UI.Icons.icons.spinner ]


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
        [ if context.config.hideThumbnails then
            Html.text ""

          else
            Html.div
                [ Html.Attributes.class "thumbnail" ]
                [ Html.img
                    [ Html.Attributes.src
                        (Constants.externalServerUrls.thumbnail document.id)
                    ]
                    []
                ]
        , Html.div [ Html.Attributes.class "description" ]
            [ Html.div [ Html.Attributes.class "header" ]
                [ Html.div [ Html.Attributes.class "header-left" ]
                    [ Html.span [ Html.Attributes.class "result-number" ]
                        [ Html.text <| String.fromInt number ++ ". " ]
                    , Html.span [ Html.Attributes.class "metadatatype" ]
                        [ Html.text document.metadatatypeName ]
                    ]
                , Html.div [ Html.Attributes.class "header-right" ]
                    [ viewSearchMatching context.config document.searchMatching ]
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
        ]


viewSearchMatching : Config -> Maybe Document.SearchMatching -> Html msg
viewSearchMatching config maybeSearchMatching =
    let
        ( noticeShort, noticeLong ) =
            case maybeSearchMatching of
                Nothing ->
                    ( "", "" )

                Just { attributes, fulltext } ->
                    Tuple.mapBoth
                        (Localization.string config)
                        (Localization.string config)
                    <|
                        case ( attributes, fulltext ) of
                            ( False, False ) ->
                                ( { en = "", de = "" }, { en = "", de = "" } )

                            ( True, False ) ->
                                ( { en = "Search term found in metadata"
                                  , de = "Fundstellen in Metadaten"
                                  }
                                , { en = "Search term found in metadata"
                                  , de = "Suchbegriff in Metadaten gefunden"
                                  }
                                )

                            ( False, True ) ->
                                ( { en = "Search term found in fulltext"
                                  , de = "Fundstellen in Volltext"
                                  }
                                , { en = "Search term found in fulltext"
                                  , de = "Suchbegriff in Volltext gefunden"
                                  }
                                )

                            ( True, True ) ->
                                ( { en = "Search term found in metadata and fulltext"
                                  , de = "Fundstellen in Metadaten und Volltext"
                                  }
                                , { en = "Search term found in metadata and fulltext"
                                  , de = "Suchbegriff in Metadaten und Volltext gefunden"
                                  }
                                )
    in
    Html.span
        [ Html.Attributes.class "found-locations"
        , Html.Attributes.title noticeLong
        ]
        [ Html.text noticeShort ]


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
                    , -- For developing and debugging purposes:
                      -- Attach the field name as a data attribute to the DOM node
                      Html.Attributes.attribute "data-mediatum-field" attribute.field
                    ]
                    (let
                        markup =
                            value
                                |> Entities.Markup.trim Constants.maxAttributeLengthInListingView
                                |> Entities.Markup.view
                                |> Html.span []
                     in
                     if isField keys.year then
                        [ value
                            |> Entities.Markup.normalizeYear
                            |> Entities.Markup.view
                            |> Html.span []
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
                , Html.Attributes.class "visual-button"
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
            , Localization.text context.config <|
                if PageSequence.hasAtLeastOneDocument pageSequence then
                    { en = "No More Results"
                    , de = "keine weiteren Ergebnisse"
                    }

                else
                    { en = "No Results"
                    , de = "keine Ergebnisse"
                    }
            ]
