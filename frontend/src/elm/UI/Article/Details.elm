module UI.Article.Details exposing
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

import Cache exposing (Cache)
import Constants
import Entities.Document as Document exposing (Attribute, Document)
import Entities.Markup as Markup exposing (Markup)
import Entities.Residence as Residence exposing (Residence)
import Html exposing (Html)
import Html.Attributes
import List.Nonempty
import Regex
import RemoteData
import Types exposing (DocumentIdFromSearch)
import Types.AdjustmentToSetup as AdjustmentToSetup exposing (AdjustmentToSetup)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.Localization as Localization
import Types.Route exposing (Route)
import UI.Icons
import UI.Widgets.Breadcrumbs
import UI.Widgets.ThumbnailSwitch
import Utils.Html
import Utils.List


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , route : Route
    , documentIdFromSearch : DocumentIdFromSearch
    }


{-| -}
type Return
    = NoReturn
    | AdjustSetup AdjustmentToSetup


{-| -}
type alias Model =
    {}


{-| -}
type Msg
    = ReturnAdjustmentToSetup AdjustmentToSetup


{-| -}
initialModel : Model
initialModel =
    {}


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        ReturnAdjustmentToSetup adjustmentToSetup ->
            ( model
            , Cmd.none
            , AdjustSetup adjustmentToSetup
            )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.article [ Html.Attributes.class "details" ]
        [ Html.div
            [ Html.Attributes.class "thumbnail-switch" ]
            [ UI.Widgets.ThumbnailSwitch.view
                context.config
                context.config.hideThumbnails
                (ReturnAdjustmentToSetup << AdjustmentToSetup.HideThumbnails)
            ]
        , case
            RemoteData.map2 Tuple.pair
                (Cache.get context.cache.documents
                    ( Config.getMaskName MasksConfig.MaskForDetails context.config
                    , context.documentIdFromSearch
                    )
                )
                (Cache.get context.cache.residence context.documentIdFromSearch.id)
          of
            RemoteData.NotAsked ->
                -- Should never happen
                UI.Icons.icons.spinner

            RemoteData.Loading ->
                UI.Icons.icons.spinner

            RemoteData.Failure error ->
                Utils.Html.viewApiError error

            RemoteData.Success ( document, residence ) ->
                viewDocument context model document residence
        ]


viewDocument : Context -> Model -> Document -> Residence -> Html Msg
viewDocument context model document residence =
    Html.div []
        [ Html.div
            [ Html.Attributes.class "header" ]
            [ Html.div [ Html.Attributes.class "metadatatype" ]
                [ Html.text document.metadatatypeName ]
            ]
        , if context.config.hideThumbnails || not (Document.hasPresentation document) then
            Html.text ""

          else
            Html.div
                [ Html.Attributes.class "thumbnail" ]
                [ (if Document.hasDocumentPdf document then
                    \html ->
                        Html.a
                            [ Html.Attributes.href (Constants.externalServerUrls.showDocumentPdf document.id)
                            , Html.Attributes.target "_blank"
                            , Localization.title context.config
                                { en = "Open fulltext in new window"
                                , de = "Volltext in neuem Fenster öffnen"
                                }
                            ]
                            [ html ]

                   else
                    identity
                  )
                    (Html.img
                        [ Html.Attributes.src
                            (Constants.externalServerUrls.presentation document.id)
                        ]
                        []
                    )
                ]
        , Html.div
            [ Html.Attributes.class "external-links" ]
            (Utils.List.appendIf
                (Document.hasDocumentPdf document)
                [ Html.a
                    [ Html.Attributes.href (Constants.externalServerUrls.downloadDocumentPdf document.id) ]
                    [ Localization.text context.config
                        { en = "Download PDF"
                        , de = "PDF herunterladen"
                        }
                    ]
                , Html.span [ Html.Attributes.class "separator" ] [ Html.text " · " ]
                ]
                [ Html.a
                    [ Html.Attributes.href
                        (Constants.externalServerUrls.documentPermanent document.id)
                    , Localization.title context.config
                        { en = "Persistent link to the document in mediaTUM"
                        , de = "Dauerhafter Verweis auf das Dokument in mediaTUM"
                        }
                    ]
                    [ Localization.text context.config
                        { en = "Show this document in mediaTUM"
                        , de = "Zeige dieses Dokument in mediaTUM"
                        }
                    ]
                ]
            )
        , Html.table []
            [ Html.tbody []
                (List.map
                    viewAttribute
                    document.attributes
                )
            ]
        , viewBibtex context.config document
        , viewSearchMatching context.config document.searchMatching
        , viewResidence context residence
        ]


viewBibtex : Config -> Document -> Html msg
viewBibtex config document =
    Html.div
        [ Html.Attributes.class "bibtex" ]
        [ Html.a
            [ Html.Attributes.href (Constants.externalServerUrls.bibtex document.id)
            , Html.Attributes.target "_blank"
            , Localization.title config
                { en = "Open BibTeX information in new window"
                , de = "BibTeX Informationen in neuem Fenster öffnen"
                }
            ]
            [ Html.img [ Html.Attributes.src Constants.externalServerUrls.bibtexLogo ] []
            , Html.text "BibTex"
            ]
        ]



-- <a href="/export/1591291/bibtex" target="bibtexdocument" title="BibTeX Informationen in neuem Fenster öffnen">
-- <img src="/img/bibtex.gif">&nbsp;BibTeX</a>


viewSearchMatching : Config -> Maybe Document.SearchMatching -> Html msg
viewSearchMatching config maybeSearchMatching =
    case maybeSearchMatching of
        Nothing ->
            Html.text ""

        Just { attributes, fulltext } ->
            let
                block translations =
                    Html.div
                        [ Html.Attributes.class "search-matching" ]
                        [ Localization.text config translations ]
            in
            case ( attributes, fulltext ) of
                ( False, False ) ->
                    Html.text ""

                ( True, False ) ->
                    block
                        { en = "Search term found in metadata"
                        , de = "Suchbegriff in Metadaten gefunden"
                        }

                ( False, True ) ->
                    block
                        { en = "Search term found in fulltext"
                        , de = "Suchbegriff in Volltext gefunden"
                        }

                ( True, True ) ->
                    block
                        { en = "Search term found in metadata and fulltext"
                        , de = "Suchbegriff in Metadaten und Volltext gefunden"
                        }


keys :
    { year : Regex.Regex
    , yearmonth : Regex.Regex
    , date : Regex.Regex
    , wwwAddress : Regex.Regex
    , urn : Regex.Regex
    , doi : Regex.Regex
    , license : Regex.Regex
    }
keys =
    let
        regex regexString =
            Maybe.withDefault Regex.never (Regex.fromString regexString)
    in
    { year = regex "year"
    , yearmonth = regex "yearmonth"
    , date = regex "date"
    , wwwAddress = regex "www-address"
    , urn = regex "^urn$"
    , doi = regex "^doi$"
    , license = regex "^license$"
    }


viewAttribute : Attribute -> Html msg
viewAttribute attribute =
    case attribute.value of
        Just value ->
            if Markup.isEmpty value then
                Html.text ""

            else
                let
                    isField regex =
                        Regex.contains regex attribute.field

                    fixedValue =
                        value
                            |> (if isField keys.yearmonth then
                                    Markup.normalizeYearMonth

                                else if isField keys.year then
                                    Markup.normalizeYear

                                else if isField keys.date then
                                    Markup.normalizeYearMonthDay

                                else if isField keys.wwwAddress then
                                    Markup.renderWwwAddress

                                else if isField keys.urn then
                                    Markup.renderUrn

                                else if isField keys.doi then
                                    Markup.renderDoi

                                else if isField keys.license then
                                    Markup.renderLicense

                                else
                                    identity
                               )
                in
                Html.tr
                    [ Html.Attributes.class "attribute" ]
                    [ Html.td
                        [ -- For developing and debugging purposes:
                          -- Attach the field name as a data attribute to the DOM node
                          Html.Attributes.attribute "data-mediatum-field" attribute.field
                        ]
                        [ Html.text attribute.name ]
                    , Html.td []
                        (Markup.view fixedValue)
                    ]

        Nothing ->
            Html.text ""


viewResidence : Context -> Residence -> Html msg
viewResidence context residence =
    let
        residenceLimitedToToplevelFolders : Residence
        residenceLimitedToToplevelFolders =
            Residence.limitToToplevelFolders context.config residence
    in
    if List.isEmpty residenceLimitedToToplevelFolders then
        Html.text ""

    else
        Html.div
            [ Html.Attributes.class "residence" ]
            [ Html.div
                [ Html.Attributes.class "title" ]
                [ Localization.text context.config
                    { en = "Occurrences:"
                    , de = "Vorkommen:"
                    }
                ]
            , Html.ul [] <|
                List.map
                    (\lineage ->
                        Html.li []
                            [ lineage
                                |> List.Nonempty.toList
                                |> Just
                                |> UI.Widgets.Breadcrumbs.view context
                            ]
                    )
                    residenceLimitedToToplevelFolders
            ]
