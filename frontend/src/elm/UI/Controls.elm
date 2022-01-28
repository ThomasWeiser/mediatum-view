module UI.Controls exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , updateFromRoute, update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs initialModel
@docs updateFromRoute, update
@docs view

-}

import Browser.Dom
import Cache exposing (Cache)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Maybe.Extra
import RemoteData
import String.Format
import Task
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config exposing (Config)
import Types.Config.FacetAspectConfig as FacetAspect
import Types.Config.FtsAspectConfig as FtsAspect
import Types.FilterList as FilterList
import Types.Localization as Localization
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation as Presentation exposing (Presentation(..))
import Types.RearrangeableEditList exposing (RearrangeableEditList, rearrange)
import Types.Route exposing (Route)
import Types.SearchTerm as SearchTerm
import Types.Selection as Selection exposing (Sorting(..))
import UI.Icons
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
    = NoOp
    | SetGlobalFtsText String
    | ClearGlobalFtsText
    | AddFtsFilter Aspect
    | SelectFtsFilter Aspect
    | SetFtsFilterText Aspect String
    | RemoveFtsFilter Aspect
    | SelectFacetFilter Aspect
    | RemoveFacetFilter Aspect
    | SetSorting Sorting
    | Submit


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
        NoOp ->
            ( model
            , Cmd.none
            , NoReturn
            )

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
            , Browser.Dom.focus (idOfAspectSearchField aspect)
                |> Task.attempt (always NoOp)
            , NoReturn
            )

        SelectFtsFilter aspect ->
            ( model
            , Browser.Dom.focus (idOfAspectSearchField aspect)
                |> Task.attempt (always NoOp)
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

        SelectFacetFilter aspect ->
            ( model
            , Cmd.none
            , NoReturn
              -- TODO
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
            , viewSearchButtons context.config model
            ]
        ]


viewSearch : Context -> Model -> Html Msg
viewSearch context model =
    Html.div [ Html.Attributes.class "search-bar" ]
        [ Html.label
            [ Html.Attributes.class "search-label" ]
            [ Localization.text context.config
                { en = "Metadata & Fulltext"
                , de = "Metadaten & Volltext"
                }
            ]
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


viewSearchButtons : Config -> Model -> Html Msg
viewSearchButtons config model =
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
            [ UI.Icons.search
            , Localization.text config
                { en = " By Rank"
                , de = " Beste zuerst"
                }
            ]
        , Html.button
            [ Html.Attributes.type_ "submit"
            , Html.Attributes.classList
                [ ( "selected"
                  , model.sorting == ByDate
                  )
                ]
            , Html.Events.onClick (SetSorting ByDate)
            ]
            [ UI.Icons.search
            , Localization.text config
                { en = " By Date"
                , de = " Neueste zuerst"
                }
            ]
        ]


getSearchFieldPlaceholder : Context -> String
getSearchFieldPlaceholder context =
    Presentation.getFolderId context.cache context.presentation
        |> Maybe.Extra.orElse
            (context.config.toplevelFolderIds |> List.head)
        |> Maybe.andThen
            (\folderId ->
                Cache.get context.cache.folders folderId
                    |> RemoteData.toMaybe
                    |> Maybe.map .name
            )
        |> Maybe.Extra.unwrap
            (Localization.string context.config
                { en = "Search"
                , de = "Suche"
                }
            )
            (\folderName ->
                Localization.string context.config
                    { en = "Search in {{}}"
                    , de = "Suche in {{}}"
                    }
                    |> String.Format.value folderName
            )


viewFtsFilters : Config -> Model -> Html Msg
viewFtsFilters config model =
    Html.div [ Html.Attributes.class "filters-bar" ]
        [ viewFtsAspectButtons config model.ftsFilterLines
        , viewExistingFtsFilters config model.ftsFilterLines
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
                (FtsAspect.getLabelOrAspectName config aspect config.ftsAspects)
            ]
        , Html.span
            [ Html.Attributes.class "input-group" ]
            [ Html.input
                [ Html.Attributes.id (idOfAspectSearchField aspect)
                , Html.Attributes.class "search-input"
                , Html.Attributes.type_ "search"
                , Html.Attributes.placeholder
                    (Localization.string config
                        { en = "Search for {{}}"
                        , de = "Suche nach {{}}"
                        }
                        |> String.Format.value
                            (FtsAspect.getLabelOrAspectName config aspect config.ftsAspects)
                    )
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


idOfAspectSearchField : Aspect -> String
idOfAspectSearchField aspect =
    "search-input-aspect-" ++ Aspect.toString aspect


viewFtsAspectButtons : Config -> List ( Aspect, String ) -> Html Msg
viewFtsAspectButtons config ftsFilterLines =
    Html.div []
        [ Localization.text config
            { en = "Search for: "
            , de = "Suchen nach: "
            }
        , Html.span
            [ Html.Attributes.class "fts-aspect-buttons" ]
            (config.ftsAspects
                |> List.map
                    (\{ aspect, label } ->
                        let
                            ftsFilterLineIsAlreadyOpen =
                                Utils.List.findByMapping Tuple.first aspect ftsFilterLines /= Nothing
                        in
                        Html.span
                            [ Html.Attributes.class "text-button"
                            , Html.Attributes.classList
                                [ ( "text-button-negligible"
                                  , ftsFilterLineIsAlreadyOpen
                                  )
                                ]
                            , Html.Events.onClick <|
                                if ftsFilterLineIsAlreadyOpen then
                                    SelectFtsFilter aspect

                                else
                                    AddFtsFilter aspect
                            ]
                            [ Localization.text config label ]
                    )
                |> List.intersperse
                    (Html.span [ Html.Attributes.class "separator" ] [ Html.text " · " ])
            )
        ]


viewFacetFilters : Context -> Html Msg
viewFacetFilters context =
    case Presentation.getSelection context.presentation of
        Nothing ->
            Html.text ""

        Just selection ->
            let
                listOfFacetFilters =
                    selection.facetFilters |> FilterList.toList
            in
            Html.div [ Html.Attributes.class "filters-bar" ]
                [ viewFacetFilterButtons context listOfFacetFilters
                , viewSelectedFacetFilters context listOfFacetFilters
                ]


viewFacetFilterButtons : Context -> List Selection.FacetFilter -> Html Msg
viewFacetFilterButtons context listOfFacetFilters =
    Html.div []
        [ Localization.text context.config
            { en = "Select: "
            , de = "Auswählen: "
            }
        , Html.span
            [ Html.Attributes.class "facet-aspect-buttons" ]
            (context.config.facetAspects
                |> List.map
                    (\{ aspect, label } ->
                        let
                            facetFilterIsAlreadyActive =
                                Utils.List.findByMapping Tuple.first aspect listOfFacetFilters /= Nothing
                        in
                        Html.span
                            [ Html.Attributes.class "text-button"
                            , Html.Attributes.classList
                                [ ( "text-button-negligible"
                                  , facetFilterIsAlreadyActive
                                  )
                                ]
                            , Html.Events.onClick <|
                                SelectFacetFilter aspect
                            ]
                            [ Localization.text context.config label ]
                    )
                |> List.intersperse
                    (Html.span [ Html.Attributes.class "separator" ] [ Html.text " · " ])
            )
        ]


viewSelectedFacetFilters : Context -> List Selection.FacetFilter -> Html Msg
viewSelectedFacetFilters context listOfFacetFilters =
    let
        listOfHtml =
            listOfFacetFilters
                |> Utils.List.mapAndMarkLast (viewFacetFilter context.config)
    in
    if List.isEmpty listOfHtml then
        Html.text ""

    else
        Html.div []
            (Html.span []
                [ Localization.text context.config
                    { en = "Selected: "
                    , de = "Ausgewählt: "
                    }
                ]
                :: listOfHtml
            )


viewFacetFilter : Config -> Bool -> Selection.FacetFilter -> Html Msg
viewFacetFilter config isLastElement ( aspect, value ) =
    Html.span
        [ Html.Attributes.class "fixed-facet-filter stick-on-wrapping"
        ]
        [ Html.span
            [ Html.Attributes.class "aspect-name" ]
            [ Html.text
                (FacetAspect.getLabelOrAspectName config aspect config.facetAspects)
            , Html.text " "
            ]
        , Html.span
            [ Html.Attributes.class "aspect-value" ]
            [ Html.text value ]
        , Html.text " "
        , Html.span
            [ Html.Attributes.class "text-button"
            , Html.Events.onClick (RemoveFacetFilter aspect)
            ]
            [ Localization.text config
                { en = "(unselect)"
                , de = "(abwählen)"
                }
            ]
        , if isLastElement then
            Html.text ""

          else
            Html.span [ Html.Attributes.class "separator" ] [ Html.text "·" ]
        ]
