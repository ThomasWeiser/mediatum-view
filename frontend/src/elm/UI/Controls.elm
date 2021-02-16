module UI.Controls exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , submitExampleQuery
    , initialModel
    , updateFromRoute, update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs submitExampleQuery
@docs initialModel
@docs updateFromRoute, update
@docs view

-}

import Cache exposing (Cache)
import Cache.Derive
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Maybe.Extra
import RemoteData
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config exposing (Config)
import Types.Config.FacetAspect as FacetAspect exposing (FacetAspect)
import Types.Config.FtsAspect as FtsAspect exposing (FtsAspect)
import Types.FilterList as FilterList
import Types.Localization as Localization
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation exposing (Presentation(..))
import Types.RearrangeableEditList exposing (RearrangeableEditList, rearrange)
import Types.Route exposing (Route)
import Types.SearchTerm as SearchTerm
import Types.Selection as Selection exposing (Sorting(..))
import UI.Icons
import Utils
import Utils.List


{-| -}
type alias Context =
    { config : Config
    , route : Route
    , cache : Cache
    , presentation : Presentation
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation


{-| -}
type alias Model =
    { globalFtsText : String
    , ftsFilterLines : RearrangeableEditList Aspect String
    , sorting : Sorting
    }


{-| -}
type Msg
    = SetGlobalFtsText String
    | ClearGlobalFtsText
    | AddFtsFilter Aspect
    | SetFtsFilterText Aspect String
    | RemoveFtsFilter Aspect
    | RemoveFacetFilter Aspect
    | SetSorting Sorting
    | Submit
    | SubmitExampleQuery


{-| -}
submitExampleQuery : Msg
submitExampleQuery =
    SubmitExampleQuery


{-| -}
initialModel : Config -> Model
initialModel config =
    { globalFtsText = ""
    , ftsFilterLines = []
    , sorting = config.defaultSorting
    }


{-| -}
updateFromRoute : Route -> Model -> Model
updateFromRoute route model =
    { model
        | globalFtsText =
            case route.parameters.globalFts of
                Nothing ->
                    ""

                Just seachTerm ->
                    SearchTerm.toString seachTerm
        , ftsFilterLines =
            rearrange
                (Tuple.second >> String.isEmpty)
                (route.parameters.ftsFilters
                    |> FilterList.toList
                    |> List.map (Tuple.mapSecond SearchTerm.toString)
                )
                model.ftsFilterLines
        , sorting = route.parameters.sorting
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        SetGlobalFtsText globalFtsText ->
            ( { model | globalFtsText = globalFtsText }
            , Cmd.none
            , NoReturn
            )

        ClearGlobalFtsText ->
            let
                model1 =
                    { model | globalFtsText = "" }
            in
            ( model1
            , Cmd.none
            , navigate model1
            )

        AddFtsFilter aspect ->
            ( { model
                | ftsFilterLines =
                    model.ftsFilterLines
                        |> Utils.List.setOnMapping Tuple.first
                            ( aspect, "" )
              }
            , Cmd.none
            , NoReturn
            )

        SetFtsFilterText aspect searchText ->
            ( { model
                | ftsFilterLines =
                    model.ftsFilterLines
                        |> Utils.List.setOnMapping Tuple.first
                            ( aspect, searchText )
              }
            , Cmd.none
            , NoReturn
            )

        RemoveFtsFilter aspect ->
            let
                model1 =
                    { model
                        | ftsFilterLines =
                            List.Extra.filterNot
                                (Tuple.first >> (==) aspect)
                                model.ftsFilterLines
                    }
            in
            ( model1
            , Cmd.none
            , navigate model1
            )

        RemoveFacetFilter aspect ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.ShowListingWithRemovedFacetFilter aspect)
            )

        SetSorting sorting ->
            ( { model | sorting = sorting }
            , Cmd.none
            , NoReturn
            )

        Submit ->
            ( model
            , Cmd.none
            , navigate model
            )

        SubmitExampleQuery ->
            let
                model1 =
                    { model
                        | globalFtsText = "variable"
                        , ftsFilterLines =
                            model.ftsFilterLines
                                |> Utils.List.setOnMapping Tuple.first
                                    ( Aspect.fromString "person", "Helmut" )
                                |> Utils.List.setOnMapping Tuple.first
                                    ( Aspect.fromString "title", "Method" )
                    }
            in
            ( model1
            , Cmd.none
            , navigate model1
            )


navigate : Model -> Return
navigate model =
    Navigate
        (Navigation.ShowListingWithSearchAndFtsFilter
            (SearchTerm.fromString model.globalFtsText)
            (model.ftsFilterLines
                |> List.filterMap
                    (\( aspect, searchText ) ->
                        SearchTerm.fromString searchText
                            |> Maybe.map (Tuple.pair aspect)
                    )
                |> Selection.ftsFiltersFromList
            )
            model.sorting
        )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.nav []
        [ Html.form
            [ Html.Events.onSubmit Submit ]
            [ viewSearch context model
            , viewFtsFilters context.config model
            , viewFacetFilters context
            , viewSearchButtons model
            ]
        ]


viewSearch : Context -> Model -> Html Msg
viewSearch context model =
    Html.div [ Html.Attributes.class "search-bar" ]
        [ Html.label
            [ Html.Attributes.class "search-label" ]
            [ Html.text "metadata & fulltext" ]
        , Html.span [ Html.Attributes.class "input-group" ]
            [ Html.input
                [ Html.Attributes.class "search-input"
                , Html.Attributes.type_ "search"
                , Html.Attributes.placeholder
                    (getSearchFieldPlaceholder context)
                , Html.Attributes.value model.globalFtsText
                , Html.Events.onInput SetGlobalFtsText
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "clear-input"
                , Html.Attributes.disabled (model.globalFtsText == "")
                , Html.Events.onClick ClearGlobalFtsText
                ]
                [ UI.Icons.clear ]
            ]
        ]


viewSearchButtons : Model -> Html Msg
viewSearchButtons model =
    Html.div [ Html.Attributes.class "submit-buttons" ]
        [ Html.button
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.classList
                [ ( "selected"
                  , model.sorting == ByRank
                  )
                ]
            , Html.Events.onClick (SetSorting ByRank)
            ]
            [ UI.Icons.search, Html.text " By Rank" ]
        , Html.button
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.classList
                [ ( "selected"
                  , model.sorting == ByDate
                  )
                ]
            , Html.Events.onClick (SetSorting ByDate)
            ]
            [ UI.Icons.search, Html.text " By Date" ]
        ]


getSearchFieldPlaceholder : Context -> String
getSearchFieldPlaceholder context =
    Types.Presentation.getFolderId context.cache context.presentation
        |> Maybe.Extra.orElse
            (Cache.Derive.getRootFolderId context.cache)
        |> Maybe.andThen
            (\folderId ->
                Cache.get context.cache.folders folderId
                    |> RemoteData.toMaybe
                    |> Maybe.map .name
            )
        |> Maybe.Extra.unwrap
            "Search"
            (\folderName -> "Search in " ++ folderName)


viewFtsFilters : Config -> Model -> Html Msg
viewFtsFilters config model =
    Html.div [ Html.Attributes.class "filters-bar" ]
        [ viewExistingFtsFilters config model.ftsFilterLines
        , viewFtsAspectButtons config.ftsAspects model.ftsFilterLines
        ]


viewExistingFtsFilters : Config -> List ( Aspect, String ) -> Html Msg
viewExistingFtsFilters config ftsFilterLines =
    Html.div [] <|
        List.map
            (\( aspect, searchText ) ->
                viewFtsFilter config aspect searchText
            )
            ftsFilterLines


viewFtsFilter : Config -> Aspect -> String -> Html Msg
viewFtsFilter config aspect searchText =
    Html.div
        [ Html.Attributes.class "search-bar"

        -- , Html.Attributes.classList [ ( "being-edited", beingEdited ) ]
        ]
        [ Html.label
            [ Html.Attributes.class "search-label" ]
            [ Html.text
                (FtsAspect.getLabelOrAspectName Localization.LangDe aspect config.ftsAspects)
            ]
        , Html.span
            [ Html.Attributes.class "input-group" ]
            [ Html.input
                [ Html.Attributes.class "search-input"
                , Html.Attributes.type_ "search"
                , Html.Attributes.placeholder <|
                    "Search "
                        ++ FtsAspect.getLabelOrAspectName Localization.LangDe aspect config.ftsAspects
                , Html.Attributes.value searchText
                , Html.Events.onInput (SetFtsFilterText aspect)
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "button"

                -- , Html.Attributes.disabled beingEdited
                , Html.Events.onClick (RemoveFtsFilter aspect)
                , Html.Attributes.class "filter-button"
                ]
                [ UI.Icons.clear ]
            ]
        ]


viewFtsAspectButtons : List FtsAspect -> List ( Aspect, String ) -> Html Msg
viewFtsAspectButtons listOfFtsAspects ftsFilterLines =
    Html.div [] <|
        List.filterMap
            (\{ aspect, label } ->
                if Utils.List.findByMapping Tuple.first aspect ftsFilterLines == Nothing then
                    Just <|
                        Html.span
                            [ Html.Attributes.class "" ]
                            [ Html.button
                                [ Html.Attributes.type_ "button"
                                , Html.Attributes.class "add-filter-button"
                                , Html.Events.onClick <| AddFtsFilter aspect
                                ]
                                [ Html.text (Localization.translation Localization.LangDe label) ]
                            ]

                else
                    Nothing
            )
            listOfFtsAspects


viewFacetFilters : Context -> Html Msg
viewFacetFilters context =
    Html.div [ Html.Attributes.class "filters-bar" ]
        (context.route.parameters.facetFilters
            |> FilterList.toList
            |> List.map (viewFacetFilter context.config)
        )


viewFacetFilter : Config -> Selection.FacetFilter -> Html Msg
viewFacetFilter config ( aspect, value ) =
    Html.div
        [ Html.Attributes.class "search-bar"
        ]
        [ Html.label
            [ Html.Attributes.class "search-label" ]
            [ Html.text
                (FacetAspect.getLabelOrAspectName Localization.LangDe aspect config.facetAspects)
            ]
        , Html.span
            [ Html.Attributes.class "input-group" ]
            [ Html.div
                [ Html.Attributes.class "search-input"
                ]
                [ Html.text value ]
            , Html.button
                [ Html.Attributes.type_ "button"

                -- , Html.Attributes.disabled beingEdited
                , Html.Events.onClick (RemoveFacetFilter aspect)
                , Html.Attributes.class "filter-button"
                ]
                [ UI.Icons.clear ]
            ]
        ]
