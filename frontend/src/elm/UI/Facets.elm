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
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config exposing (Config)
import Types.Config.FacetAspectConfig as FacetAspect exposing (FacetAspectConfig)
import Types.FacetValue exposing (FacetValues)
import Types.FilterList as FilterList
import Types.Localization as Localization
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation exposing (Presentation(..))
import Types.Selection exposing (Selection)
import UI.Icons
import Utils
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
    { showLongList : Sort.Dict.Dict Aspect Bool }


{-| -}
type Msg
    = SelectFacetValue Aspect String
    | SelectFacetUnfilter Aspect
    | ShowFacetLongList Aspect Bool


{-| -}
initialModel : Model
initialModel =
    { showLongList = Sort.Dict.empty (Utils.sorter Aspect.ordering) }


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

        ShowFacetLongList aspect state ->
            ( { model
                | showLongList = Sort.Dict.insert aspect state model.showLongList
              }
            , Cmd.none
            , NoReturn
            )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.div
        [ Html.Attributes.class "facets-bar" ]
    <|
        case context.presentation of
            ListingPresentation selection _ ->
                viewFacets context model selection

            IteratorPresentation selection _ _ ->
                viewFacets context model selection

            _ ->
                [ Html.text "" ]


viewFacets : Context -> Model -> Selection -> List (Html Msg)
viewFacets context model selection =
    List.map
        (viewFacet context model selection)
        context.config.facetAspects


viewFacet : Context -> Model -> Selection -> FacetAspectConfig -> Html Msg
viewFacet context model selection facetAspectConfig =
    Html.nav
        [ Html.Attributes.class "facet-box" ]
        [ Html.div
            [ Html.Attributes.class "facet-name" ]
            [ Localization.text context.config facetAspectConfig.label ]
        , Html.div
            []
            (case FilterList.get facetAspectConfig.aspect selection.facetFilters of
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
                            [ UI.Icons.spinnerSmall ]

                        RemoteData.Loading ->
                            [ UI.Icons.spinnerSmall ]

                        RemoteData.Failure error ->
                            [ Utils.Html.viewApiError error ]

                        RemoteData.Success facetsValues ->
                            viewFacetValues
                                context.config
                                model
                                facetAspectConfig.aspect
                                (Sort.Dict.get facetAspectConfig.aspect facetsValues
                                    |> Maybe.withDefault []
                                )
            )
        ]


viewFacetSelection : Config -> Aspect -> String -> Maybe Int -> List (Html Msg)
viewFacetSelection config aspect selectedValue maybeCount =
    [ Html.div
        [ Html.Attributes.class "facet-line facet-clickable facet-special-action"
        , Html.Events.onClick (SelectFacetUnfilter aspect)
        ]
        [ Html.span
            [ Html.Attributes.class "facet-value-text" ]
            [ Localization.text config
                { en = "<< All"
                , de = "<< zurück"
                }
            ]
        ]
    , Html.ul
        [ Html.Attributes.class "facet-values" ]
        [ Html.li
            [ Html.Attributes.class "facet-line"
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
    ]


viewFacetValues : Config -> Model -> Aspect -> FacetValues -> List (Html Msg)
viewFacetValues config model aspect facetValues =
    let
        showShortList =
            Sort.Dict.get aspect model.showLongList
                |> Maybe.withDefault False
                |> not
    in
    [ Html.ul
        [ Html.Attributes.class "facet-values" ]
      <|
        List.indexedMap
            (\position { value, count } ->
                if showShortList && position >= config.numberOfFacetValuesShortList then
                    Html.text ""

                else
                    Html.li
                        [ Html.Attributes.class "facet-line facet-clickable"
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
    , if List.length facetValues <= config.numberOfFacetValuesShortList then
        Html.text ""

      else
        Html.div
            [ Html.Attributes.class "facet-line facet-clickable facet-special-action"
            , Html.Events.onClick (ShowFacetLongList aspect showShortList)
            ]
            [ Html.span
                [ Html.Attributes.class "facet-value-text" ]
                [ if showShortList then
                    Localization.text config
                        { en = ">> More"
                        , de = ">> mehr"
                        }

                  else
                    Localization.text config
                        { en = "<< Less"
                        , de = "<< weniger"
                        }
                ]
            ]
    ]


viewNotSpecified : Config -> Html msg
viewNotSpecified config =
    Html.i []
        [ Localization.text config
            { en = "[not specified]"
            , de = "[nicht angegeben]"
            }
        ]
