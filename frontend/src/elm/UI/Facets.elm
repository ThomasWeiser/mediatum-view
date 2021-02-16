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
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import RemoteData
import Sort.Dict
import String.Extra
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config exposing (Config)
import Types.FacetValue exposing (FacetValues)
import Types.FilterList as FilterList exposing (FilterList)
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
    , facetAspects : List Aspect
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation
    | ChangedFacetAspects (List Aspect)


{-| -}
type alias Model =
    { facetAspectsInput : String
    }


{-| -}
type Msg
    = SetFacetAspectsInput String
    | SelectFacetValue Aspect String
    | SelectFacetUnfilter Aspect


{-| -}
initialModel : List Aspect -> Model
initialModel facetAspects =
    { facetAspectsInput =
        facetAspects
            |> List.map Aspect.toString
            |> String.join " "
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

        SetFacetAspectsInput facetAspectsInput ->
            ( { model | facetAspectsInput = facetAspectsInput }
            , Cmd.none
            , facetAspectsInput
                |> String.Extra.clean
                |> String.split " "
                |> List.filter (String.Extra.isBlank >> not)
                |> List.map Aspect.fromString
                |> ChangedFacetAspects
            )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.div []
        [ viewFacets context
        , viewFacetAspectsInput model
        ]


viewFacetAspectsInput : Model -> Html Msg
viewFacetAspectsInput model =
    Html.div []
        [ Html.input
            [ Html.Attributes.class "facet-aspects-input"
            , Html.Attributes.type_ "text"
            , Html.Attributes.placeholder "Facet Aspects ..."
            , Html.Attributes.value model.facetAspectsInput
            , Utils.onChange SetFacetAspectsInput
            ]
            []
        ]


viewFacets : Context -> Html Msg
viewFacets context =
    case context.presentation of
        ListingPresentation selection _ ->
            Html.div
                [ Html.Attributes.class "facets-bar" ]
                (List.map
                    (viewFacet context selection)
                    context.facetAspects
                )

        _ ->
            Html.div [ Html.Attributes.class "facets-bar" ]
                [ Html.text ""
                ]


viewFacet : Context -> Selection -> Aspect -> Html Msg
viewFacet context selection aspect =
    Html.nav
        [ Html.Attributes.class "facet-box" ]
        [ Html.div
            [ Html.Attributes.class "facet-name" ]
            [ Html.text (Aspect.toString aspect) ]
        , Html.div
            [ Html.Attributes.class "facet-values" ]
            [ case FilterList.get aspect selection.facetFilters of
                Just selectedValue ->
                    viewFacetSelection
                        aspect
                        selectedValue
                        (Cache.Derive.getDocumentCount context.cache selection
                            |> RemoteData.toMaybe
                        )

                Nothing ->
                    case
                        Cache.get
                            context.cache.facetsValues
                            ( selection, context.facetAspects )
                    of
                        RemoteData.NotAsked ->
                            -- Should never happen
                            UI.Icons.spinner

                        RemoteData.Loading ->
                            UI.Icons.spinner

                        RemoteData.Failure error ->
                            Utils.Html.viewApiError error

                        RemoteData.Success facetsValues ->
                            viewFacetValues
                                aspect
                                (Sort.Dict.get aspect facetsValues
                                    |> Maybe.withDefault []
                                )
            ]
        ]


viewFacetSelection : Aspect -> String -> Maybe Int -> Html Msg
viewFacetSelection aspect selectedValue maybeCount =
    Html.ul [] <|
        [ Html.li
            [ Html.Attributes.class "facet-value-line facet-remove-filter"
            , Html.Events.onClick (SelectFacetUnfilter aspect)
            ]
            [ Html.span
                [ Html.Attributes.class "facet-value-text" ]
                [ Html.i [] [ Html.text "<< All" ] ]
            ]
        , Html.li
            [ Html.Attributes.class "facet-value-line"
            , Html.Attributes.class "facet-value-selected"
            ]
            [ Html.span
                [ Html.Attributes.class "facet-value-text" ]
                [ if String.isEmpty selectedValue then
                    Html.i [] [ Html.text "[not specified]" ]

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


viewFacetValues : Aspect -> FacetValues -> Html Msg
viewFacetValues aspect facetValues =
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
                            Html.i [] [ Html.text "[not specified]" ]

                          else
                            Html.text value
                        ]
                    , Html.span
                        [ Html.Attributes.class "facet-value-count" ]
                        [ Html.text <| "(" ++ String.fromInt count ++ ")" ]
                    ]
            )
            facetValues
