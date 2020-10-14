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
    , facetKeys : List String
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation
    | ChangedFacetKeys (List String)


{-| -}
type alias Model =
    { facetKeysInput : String
    }


{-| -}
type Msg
    = SetFacetKeysInput String
    | SelectFacetValue String String
    | SelectFacetUnfilter String


{-| -}
initialModel : List String -> Model
initialModel facetKeys =
    { facetKeysInput = String.join " " facetKeys
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        SelectFacetValue key value ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.ShowListingWithAddedFacetFilter key value)
            )

        SelectFacetUnfilter key ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.ShowListingWithRemovedFacetFilter key)
            )

        SetFacetKeysInput facetKeysInput ->
            ( { model | facetKeysInput = facetKeysInput }
            , Cmd.none
            , facetKeysInput
                |> String.Extra.clean
                |> String.split " "
                |> List.filter (String.Extra.isBlank >> not)
                |> ChangedFacetKeys
            )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.div []
        [ viewFacets context
        , viewFacetKeysInput model
        ]


viewFacetKeysInput : Model -> Html Msg
viewFacetKeysInput model =
    Html.div []
        [ Html.input
            [ Html.Attributes.class "facet-keys-input"
            , Html.Attributes.type_ "text"
            , Html.Attributes.placeholder "Facet Keys ..."
            , Html.Attributes.value model.facetKeysInput
            , Utils.onChange SetFacetKeysInput
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
                    context.facetKeys
                )

        _ ->
            Html.div [ Html.Attributes.class "facets-bar" ]
                [ Html.text ""
                ]


viewFacet : Context -> Selection -> String -> Html Msg
viewFacet context selection key =
    Html.nav
        [ Html.Attributes.class "facet-box" ]
        [ Html.div
            [ Html.Attributes.class "facet-name" ]
            [ Html.text key ]
        , Html.div
            [ Html.Attributes.class "facet-values" ]
            [ case
                Cache.get
                    context.cache.facetsValues
                    ( selection, key )
              of
                RemoteData.NotAsked ->
                    -- Should never happen
                    UI.Icons.spinner

                RemoteData.Loading ->
                    UI.Icons.spinner

                RemoteData.Failure error ->
                    Utils.Html.viewApiError error

                RemoteData.Success facetValues ->
                    viewFacetValues
                        key
                        facetValues
                        (Dict.get key selection.facetFilters)
            ]
        ]


viewFacetValues : String -> FacetValues -> Maybe String -> Html Msg
viewFacetValues key facetValues maybeSelectedValue =
    Html.ul [] <|
        (if maybeSelectedValue == Nothing then
            Html.text ""

         else
            Html.li
                [ Html.Attributes.class "facet-value-line facet-remove-filter"
                , Html.Events.onClick (SelectFacetUnfilter key)
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
                        , Html.Events.onClick (SelectFacetValue key value)
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
