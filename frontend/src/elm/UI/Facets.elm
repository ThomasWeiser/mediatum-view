module UI.Facets exposing
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
import Cache.Derive
import Html exposing (Html)
import Html.Attributes
import Html.Events
import RemoteData
import Sort.Dict
import Types.Aspect exposing (Aspect)
import Types.Config exposing (Config)
import Types.Config.FacetAspectConfig as FacetAspect exposing (FacetAspectConfig)
import Types.FacetValue exposing (FacetValues)
import Types.FilterList as FilterList
import Types.Localization as Localization
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation exposing (Presentation(..))
import Types.Selection exposing (Selection)
import UI.Icons
import Utils.Html


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , presentation : Presentation
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation


{-| -}
type alias Model =
    ()


{-| -}
type Msg
    = SelectFacetValue Aspect String
    | SelectFacetUnfilter Aspect


{-| -}
initialModel : Model
initialModel =
    ()


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        SelectFacetValue aspect value ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.ShowListingWithAddedFacetFilter aspect value)
            )

        SelectFacetUnfilter aspect ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.ShowListingWithRemovedFacetFilter aspect)
            )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    case context.presentation of
        ListingPresentation selection _ ->
            Html.div
                [ Html.Attributes.class "facets-bar" ]
                (List.map
                    (viewFacet context selection)
                    context.config.facetAspects
                )

        _ ->
            Html.div [ Html.Attributes.class "facets-bar" ]
                [ Html.text ""
                ]


viewFacet : Context -> Selection -> FacetAspectConfig -> Html Msg
viewFacet context selection facetAspectConfig =
    Html.nav
        [ Html.Attributes.class "facet-box" ]
        [ Html.div
            [ Html.Attributes.class "facet-name" ]
            [ Localization.text context.config facetAspectConfig.label ]
        , Html.div
            [ Html.Attributes.class "facet-values" ]
            [ case FilterList.get facetAspectConfig.aspect selection.facetFilters of
                Just selectedValue ->
                    viewFacetSelection
                        context.config
                        facetAspectConfig.aspect
                        selectedValue
                        (Cache.Derive.getDocumentCount context.cache selection
                            |> RemoteData.toMaybe
                        )

                Nothing ->
                    case
                        Cache.get
                            context.cache.facetsValues
                            ( selection
                            , FacetAspect.aspects context.config.facetAspects
                            )
                    of
                        RemoteData.NotAsked ->
                            -- Should never happen
                            UI.Icons.spinnerSmall

                        RemoteData.Loading ->
                            UI.Icons.spinnerSmall

                        RemoteData.Failure error ->
                            Utils.Html.viewApiError error

                        RemoteData.Success facetsValues ->
                            viewFacetValues
                                context.config
                                facetAspectConfig.aspect
                                (Sort.Dict.get facetAspectConfig.aspect facetsValues
                                    |> Maybe.withDefault []
                                )
            ]
        ]


viewFacetSelection : Config -> Aspect -> String -> Maybe Int -> Html Msg
viewFacetSelection config aspect selectedValue maybeCount =
    Html.ul [] <|
        [ Html.li
            [ Html.Attributes.class "facet-value-line facet-remove-filter"
            , Html.Events.onClick (SelectFacetUnfilter aspect)
            ]
            [ Html.span
                [ Html.Attributes.class "facet-value-text" ]
                [ Html.i []
                    [ Localization.text config
                        { en = "<< All"
                        , de = "<< zurück"
                        }
                    ]
                ]
            ]
        , Html.li
            [ Html.Attributes.class "facet-value-line"
            , Html.Attributes.class "facet-value-selected"
            ]
            [ Html.span
                [ Html.Attributes.class "facet-value-text" ]
                [ if String.isEmpty selectedValue then
                    viewNotSpecified config

                  else
                    Html.text selectedValue
                ]
            , case maybeCount of
                Just count ->
                    Html.span
                        [ Html.Attributes.class "facet-value-count" ]
                        [ Html.text <| "(" ++ String.fromInt count ++ ")" ]

                Nothing ->
                    Html.text ""
            ]
        ]


viewFacetValues : Config -> Aspect -> FacetValues -> Html Msg
viewFacetValues config aspect facetValues =
    Html.ul [] <|
        List.map
            (\{ value, count } ->
                Html.li
                    [ Html.Attributes.class "facet-value-line"
                    , Html.Events.onClick (SelectFacetValue aspect value)
                    ]
                    [ Html.span
                        [ Html.Attributes.class "facet-value-text" ]
                        [ if String.isEmpty value then
                            viewNotSpecified config

                          else
                            Html.text value
                        ]
                    , Html.span
                        [ Html.Attributes.class "facet-value-count" ]
                        [ Html.text <| "(" ++ String.fromInt count ++ ")" ]
                    ]
            )
            facetValues


viewNotSpecified : Config -> Html msg
viewNotSpecified config =
    Html.i []
        [ Localization.text config
            { en = "[not specified]"
            , de = "[nicht angegeben]"
            }
        ]
