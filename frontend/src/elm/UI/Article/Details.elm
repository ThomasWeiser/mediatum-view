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
import Entities.Document as Document exposing (Document)
import Entities.Markup
import Entities.Residence as Residence exposing (Residence)
import Html exposing (Html)
import Html.Attributes
import List.Nonempty
import RemoteData
import Types exposing (DocumentIdFromSearch)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.Localization as Localization
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
        [ Html.div [ Html.Attributes.class "header" ]
            [ Html.div [ Html.Attributes.class "metadatatype" ]
                [ Html.text document.metadatatypeName ]
            , Html.div [ Html.Attributes.class "author" ]
                [ Html.text document.name ]
            ]
        , Html.table []
            [ Html.tbody [] <|
                List.map
                    viewAttribute
                    document.attributes
            ]
        , viewSearchMatching context.config document.searchMatching
        , viewResidence context residence
        ]


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


viewAttribute : Document.Attribute -> Html msg
viewAttribute attribute =
    case attribute.value of
        Just value ->
            if Entities.Markup.isEmpty value then
                Html.text ""

            else
                Html.tr
                    [ Html.Attributes.class "attribute" ]
                    [ Html.td [] [ Html.text attribute.name ]
                    , Html.td []
                        (Entities.Markup.view value)
                    ]

        Nothing ->
            Html.text ""


viewResidence : Context -> Residence -> Html msg
viewResidence context residence =
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
                (Residence.limitToToplevelFolders context.config residence)
        ]
