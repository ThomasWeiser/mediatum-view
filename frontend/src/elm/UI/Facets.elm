port module UI.Facets exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , update, focusOnFacet
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs initialModel
@docs update, focusOnFacet
@docs view

-}

import Cache exposing (Cache)
import Cache.Derive
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Process
import RemoteData
import Sort.Dict
import Task
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
    { showLongList : Sort.Dict.Dict Aspect Bool
    , showCollapsed : Sort.Dict.Dict Aspect Bool
    }


{-| -}
type Msg
    = SelectFacetValue Aspect String
    | SelectFacetUnfilter Aspect
    | ShowFacetCollapsed Aspect Bool
    | ShowFacetLongList Aspect Bool
    | FocusOnFacet Aspect


{-| -}
initialModel : Model
initialModel =
    { showLongList = Sort.Dict.empty (Utils.sorter Aspect.ordering)
    , showCollapsed = Sort.Dict.empty (Utils.sorter Aspect.ordering)
    }


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

        ShowFacetCollapsed aspect state ->
            ( { model
                | showCollapsed = Sort.Dict.insert aspect state model.showCollapsed
              }
            , Cmd.none
            , NoReturn
            )

        ShowFacetLongList aspect state ->
            ( { model
                | showLongList = Sort.Dict.insert aspect state model.showLongList
              }
            , Cmd.none
            , NoReturn
            )

        FocusOnFacet aspect ->
            ( model
            , scrollElementIntoView (idOfFacetBox aspect)
            , NoReturn
            )


{-| -}
focusOnFacet : Context -> Aspect -> Model -> ( Model, Cmd Msg )
focusOnFacet context aspect model =
    let
        _ =
            Debug.log "focusOnFacet" aspect
    in
    ( { model
        | showCollapsed = Sort.Dict.insert aspect False model.showCollapsed
      }
    , -- Wait for the changed DOM (i.e. not collapsed) to be drawn
      Process.sleep 50
        |> Task.perform (always (FocusOnFacet aspect))
    )


port scrollElementIntoView : String -> Cmd msg


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
    let
        showCollapsed =
            Sort.Dict.get facetAspectConfig.aspect model.showCollapsed
                |> Maybe.withDefault False
    in
    Html.nav
        [ Html.Attributes.id (idOfFacetBox facetAspectConfig.aspect)
        , Html.Attributes.class "facet-box"
        ]
        [ Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "text-button facet-head facet-clickable"
            , Html.Events.onClick (ShowFacetCollapsed facetAspectConfig.aspect (not showCollapsed))
            , Html.Attributes.classList
                [ ( "expanded", not showCollapsed ) ]
            ]
            [ Html.div []
                [ UI.Icons.expando ]
            , Html.div
                [ Html.Attributes.class "facet-name" ]
                [ Localization.text context.config facetAspectConfig.label ]
            ]
        , if showCollapsed then
            Html.text ""

          else
            Html.div
                [ Html.Attributes.class "facet-body" ]
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


idOfFacetBox : Aspect -> String
idOfFacetBox aspect =
    "facet-box-aspect-" ++ Aspect.toString aspect


viewFacetSelection : Config -> Aspect -> String -> Maybe Int -> List (Html Msg)
viewFacetSelection config aspect selectedValue maybeCount =
    [ Html.div
        [ Html.Attributes.class "facet-line facet-clickable facet-special-action"
        , Html.Events.onClick (SelectFacetUnfilter aspect)
        ]
        [ Html.span
            [ Html.Attributes.class "facet-value-text" ]
            [ Localization.text config
                { en = "<< Any"
                , de = "<< beliebig"
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
                        [ Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Attributes.class "text-button"
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
            [ Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "text-button"
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
    ]


viewNotSpecified : Config -> Html msg
viewNotSpecified config =
    Html.i []
        [ Localization.text config
            { en = "[not specified]"
            , de = "[nicht angegeben]"
            }
        ]
