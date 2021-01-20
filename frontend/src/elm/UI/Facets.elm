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
import String.Extra
import Types.Facet exposing (FacetValues)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation exposing (Presentation(..))
import Types.Selection exposing (Filter(..), FtsSorting(..), Selection)
import UI.Icons
import Utils
import Utils.Html


{-| -}
type alias Context =
    { cache : Cache
    , presentation : Presentation
    , facetAspects : List String
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation
    | ChangedFacetAspects (List String)


{-| -}
type alias Model =
    { facetAspectsInput : String
    }


{-| -}
type Msg
    = SetFacetAspectsInput String
    | SelectFacetValue String String
    | SelectFacetUnfilter String


{-| -}
initialModel : List String -> Model
initialModel facetAspects =
    { facetAspectsInput = String.join " " facetAspects
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


viewFacet : Context -> Selection -> String -> Html Msg
viewFacet context selection aspect =
    Html.nav
        [ Html.Attributes.class "facet-box" ]
        [ Html.div
            [ Html.Attributes.class "facet-name" ]
            [ Html.text aspect ]
        , Html.div
            [ Html.Attributes.class "facet-values" ]
            [ case Dict.get aspect selection.facetFilters of
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
                                (Dict.get aspect facetsValues |> Maybe.withDefault [])
                                (Dict.get aspect selection.facetFilters)
            ]
        ]


viewFacetSelection : String -> String -> Maybe Int -> Html Msg
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


viewFacetValues : String -> FacetValues -> Maybe String -> Html Msg
viewFacetValues aspect facetValues maybeSelectedValue =
    -- TODO: Remove maybeSelectedValue
    Html.ul [] <|
        (if maybeSelectedValue == Nothing then
            Html.text ""

         else
            Html.li
                [ Html.Attributes.class "facet-value-line facet-remove-filter"
                , Html.Events.onClick (SelectFacetUnfilter aspect)
                ]
                [ Html.span
                    [ Html.Attributes.class "facet-value-text" ]
                    [ Html.i [] [ Html.text "<< All" ] ]
                ]
        )
            :: List.map
                (\{ value, count } ->
                    Html.li
                        [ Html.Attributes.class "facet-value-line"
                        , Html.Attributes.classList [ ( "facet-value-selected", maybeSelectedValue == Just value ) ]
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
