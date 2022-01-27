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
import Entities.Document as Document exposing (Document)
import Entities.Markup as Markup exposing (Markup)
import Entities.Residence as Residence exposing (Residence)
import Html exposing (Html)
import Html.Attributes
import List.Nonempty
import Regex
import RemoteData
import Types exposing (DocumentIdFromSearch)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.Localization as Localization
import Types.Navigation as Navigation
import Types.Route exposing (Route)
import UI.Icons
import UI.Widgets.Breadcrumbs
import Utils.Html


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


{-| -}
type alias Model =
    {}


{-| -}
type alias Msg =
    Never


{-| -}
initialModel : Model
initialModel =
    {}


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    ( model, Cmd.none, NoReturn )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "details" ]
        [ case
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
                UI.Icons.spinner

            RemoteData.Loading ->
                UI.Icons.spinner

            RemoteData.Failure error ->
                Utils.Html.viewApiError error

            RemoteData.Success ( document, residence ) ->
                viewDocument context model document residence
        ]


viewDocument : Context -> Model -> Document -> Residence -> Html Msg
viewDocument context model document residence =
    Html.div []
        [ Html.div [ Html.Attributes.class "permalink" ]
            [ Html.a
                [ Navigation.alterRouteHref
                    context
                    (Navigation.ShowDocumentPermalink document.id)
                ]
                [ Localization.text context.config
                    { en = "Permanent link"
                    , de = "Dauerhafter Link"
                    }
                ]
            ]
        , if context.config.hideThumbnails then
            Html.text ""

          else
            Html.div
                [ Html.Attributes.class "thumbnail" ]
                [ Html.img
                    [ Html.Attributes.src
                        (Constants.externalServerUrls.presentation document.id)
                    ]
                    []
                ]
        , Html.div [ Html.Attributes.class "header" ]
            [ Html.div [ Html.Attributes.class "metadatatype" ]
                [ Html.text document.metadatatypeName ]
            ]
        , Html.table []
            [ Html.tbody []
                (viewPotentialDissertationAuthor context.config document
                    :: List.map
                        viewAttribute
                        document.attributes
                )
            ]
        , viewSearchMatching context.config document.searchMatching
        , viewResidence context residence
        ]


{-| Work around a peculiarity of the current TUM database, regarding documents with schema `diss`.

When queried by mask `nodebig` or `nodebig_en`, there is no maskitem which contains the author's name.
But for most documents with schema `diss` the author's name is given by the document's name,
which is a separate column in table `mediatum.node`.

So, in these cases we add a synthetical attribute to display the author's name.

-}
viewPotentialDissertationAuthor : Config -> Document -> Html Msg
viewPotentialDissertationAuthor config document =
    if document.metadatatypeName == "Dissertation" then
        viewAttribute
            { field = "author-from-document-name"
            , name =
                Localization.string config { en = "Author", de = "Autor" }
            , value =
                Just
                    (Markup.parse
                        (Markup.SpanClass "unparsable")
                        document.name
                    )
            }

    else
        Html.text ""


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
    }


viewAttribute : { a | name : String, value : Maybe Markup, field : String } -> Html msg
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
