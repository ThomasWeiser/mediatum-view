module UI.Controls exposing
    ( Context
    , Model
    , Msg
    , Return(..)
    , initialModel
    , submitExampleQuery
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Navigation exposing (Navigation)
import Presentation exposing (Presentation)
import Query.Filter as Filter
import Query.FilterEditor as FilterEditor
import Query.Filters as Filters
import Range
import Route exposing (Route)
import Types.SearchTerm as SearchTerm
import Types.Selection as Selection exposing (Filter(..), Filters, FtsSorting(..))
import Utils


type alias Context =
    { route : Route
    , presentation : Presentation
    }


type Return
    = NoReturn
    | Navigate Navigation


type alias Model =
    { ftsTerm : String
    , ftsSorting : FtsSorting
    , filterEditors : Dict String FilterEditor.Model
    }


type Msg
    = SetSearchTerm String
    | SetSorting FtsSorting
    | AddFilter Filter.FilterType
    | EditFilter Filter
    | RemoveFilter Filter
    | Submit
    | SubmitExampleQuery
    | FilterEditorMsg String FilterEditor.Msg


submitExampleQuery : Msg
submitExampleQuery =
    SubmitExampleQuery


initialModel : Route -> Model
initialModel route =
    { ftsTerm =
        case route.parameters.ftsTerm of
            Nothing ->
                ""

            Just seachTerm ->
                SearchTerm.toString seachTerm
    , ftsSorting = route.parameters.ftsSorting
    , filterEditors = Dict.empty
    }


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    let
        removeFilter filterHandle =
            Filters.fromRoute context.route
                |> Selection.removeFilter filterHandle
                |> Navigation.ShowListingWithFilters
                |> Navigate

        insertFilter oldFilterHandlefilter newFilter =
            Filters.fromRoute context.route
                |> Selection.removeFilter oldFilterHandlefilter
                |> Selection.insertFilter newFilter
                |> Navigation.ShowListingWithFilters
                |> Navigate
    in
    case msg of
        SetSearchTerm ftsTerm ->
            ( { model | ftsTerm = ftsTerm }
            , Cmd.none
            , NoReturn
            )

        SetSorting ftsSorting ->
            ( { model | ftsSorting = ftsSorting }
            , Cmd.none
            , NoReturn
            )

        AddFilter filterType ->
            let
                ( filterEditorModel, filterEditorCmd ) =
                    FilterEditor.init filterType.initControls

                theFilterHandle =
                    "new-" ++ filterType.name
            in
            ( { model
                | filterEditors =
                    Dict.insert theFilterHandle filterEditorModel model.filterEditors
              }
            , filterEditorCmd |> Cmd.map (FilterEditorMsg theFilterHandle)
            , NoReturn
            )

        EditFilter filter ->
            let
                ( filterEditorModel, filterEditorCmd ) =
                    FilterEditor.init (Filter.controlsFromFilter filter)

                filterHandle =
                    Selection.filterHandle filter
            in
            ( { model
                | filterEditors =
                    Dict.insert filterHandle filterEditorModel model.filterEditors
              }
            , filterEditorCmd |> Cmd.map (FilterEditorMsg filterHandle)
            , NoReturn
            )

        RemoveFilter filter ->
            ( model
            , Cmd.none
            , removeFilter (Selection.filterHandle filter)
            )

        Submit ->
            ( model
            , Cmd.none
            , Navigate
                (Navigation.ShowListingWithSearch
                    (SearchTerm.fromString model.ftsTerm)
                    model.ftsSorting
                )
            )

        SubmitExampleQuery ->
            let
                filters =
                    Selection.filtersNone
                        |> Selection.insertFilter
                            (FilterYearWithin (Range.fromTo ( 2000, 2010 )))
                        |> Selection.insertFilter
                            (FilterTitleFts
                                (SearchTerm.fromStringWithDefault "no-default-needed" "with")
                            )
            in
            ( model
            , Cmd.none
            , [ Navigation.ShowListingWithSearch
                    (SearchTerm.fromString "variable")
                    context.route.parameters.ftsSorting
              , Navigation.ShowListingWithFilters
                    filters
              ]
                |> Navigation.ListOfNavigations
                |> Navigate
            )

        FilterEditorMsg filterHandle subMsg ->
            case Dict.get filterHandle model.filterEditors of
                Just filterEditor ->
                    let
                        ( subModel, subCmd, subReturn ) =
                            FilterEditor.update subMsg filterEditor

                        modelWithEditorClosed =
                            { model
                                | filterEditors =
                                    Dict.remove filterHandle model.filterEditors
                            }

                        cmd =
                            subCmd |> Cmd.map (FilterEditorMsg filterHandle)
                    in
                    case subReturn of
                        FilterEditor.NoReturn ->
                            ( { model
                                | filterEditors =
                                    Dict.insert filterHandle subModel model.filterEditors
                              }
                            , cmd
                            , NoReturn
                            )

                        FilterEditor.Saved newFilter ->
                            ( modelWithEditorClosed
                            , cmd
                            , insertFilter filterHandle newFilter
                            )

                        FilterEditor.Removed ->
                            ( modelWithEditorClosed
                            , cmd
                            , removeFilter filterHandle
                            )

                        FilterEditor.Canceled ->
                            ( modelWithEditorClosed
                            , cmd
                            , NoReturn
                            )

                Nothing ->
                    ( model, Cmd.none, NoReturn )


view : Context -> Model -> Html Msg
view context model =
    Html.div []
        [ viewSearch context model
        , viewFilters context model
        ]


viewSearch : Context -> Model -> Html Msg
viewSearch context model =
    Html.form
        [ Html.Events.onSubmit Submit ]
        [ Html.div [ Html.Attributes.class "search-bar input-group" ]
            [ Html.input
                [ Html.Attributes.class "search-input"
                , Html.Attributes.type_ "search"
                , Html.Attributes.placeholder "Search ..."
                , Html.Attributes.value model.ftsTerm
                , Utils.onChange SetSearchTerm
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.classList
                    [ ( "selected"
                      , model.ftsSorting == FtsByRank
                      )
                    ]
                , Html.Events.onClick (SetSorting FtsByRank)
                ]
                [ Icons.search, Html.text " By Rank" ]
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.classList
                    [ ( "selected"
                      , model.ftsSorting == FtsByDate
                      )
                    ]
                , Html.Events.onClick (SetSorting FtsByDate)
                ]
                [ Icons.search, Html.text " By Date" ]
            ]
        ]


viewFilters : Context -> Model -> Html Msg
viewFilters context model =
    Html.div [ Html.Attributes.class "filters-bar" ]
        [ Html.span [] <|
            List.map
                (\filterType ->
                    Html.span
                        [ Html.Attributes.class "input-group" ]
                        [ Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Events.onClick <| AddFilter filterType
                            , Html.Attributes.class "filter-button"
                            ]
                            [ Html.text <| filterType.name ++ "..." ]
                        ]
                )
                Filter.filterTypes
        , viewExistingFilters
            model
            (Filters.fromRoute context.route)
        , Html.span [] <|
            List.map
                (\( filterHandle, filterEditor ) ->
                    FilterEditor.view filterEditor
                        |> Html.map (FilterEditorMsg filterHandle)
                )
                (Dict.toList model.filterEditors)
        ]


viewExistingFilters : Model -> Filters -> Html Msg
viewExistingFilters model filters =
    Html.span [] <|
        List.map
            (\filter ->
                let
                    beingEdited =
                        Dict.member
                            (Selection.filterHandle filter)
                            model.filterEditors
                in
                viewExistingFilter beingEdited filter
            )
            (Selection.filtersToList filters)


viewExistingFilter : Bool -> Filter -> Html Msg
viewExistingFilter beingEdited filter =
    Html.span
        [ Html.Attributes.class "input-group"
        , Html.Attributes.classList [ ( "being-edited", beingEdited ) ]
        ]
        [ Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.disabled beingEdited
            , Html.Events.onClick (EditFilter filter)
            , Html.Attributes.class "filter-button"
            ]
            (Filter.view filter)
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.disabled beingEdited
            , Html.Events.onClick (RemoveFilter filter)
            , Html.Attributes.class "filter-button"
            ]
            [ Html.text "X" ]
        ]
