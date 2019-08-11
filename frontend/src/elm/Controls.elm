module Controls exposing
    ( Context
    , Model
    , Msg
    , Return(..)
    , init
    , submitExampleQuery
    , update
    , view
    )

import Data.Types exposing (Filter(..), Filters)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import List.Extra
import Maybe.Extra
import Query exposing (Query)
import Query.Filter as Filter
import Query.FilterEditor as FilterEditor
import Query.Filters as Filters
import Tree
import Utils


type alias Context =
    { query : Query
    }


type Return
    = NoReturn
    | MapQuery (Query -> Query)


type alias Model =
    { searchTerm : String
    , sorting : Query.FtsSorting
    , filterEditors : Dict String FilterEditor.Model
    }


type Msg
    = NoOp
    | SetSearchTerm String
    | SetSorting Query.FtsSorting
    | EditFilter Filter
    | RemoveFilter Filter
    | Submit
    | SubmitExampleQuery
    | FilterEditorMsg String FilterEditor.Msg


submitExampleQuery : Msg
submitExampleQuery =
    SubmitExampleQuery


init : () -> Model
init _ =
    { searchTerm = ""
    , sorting = Query.ByRank
    , filterEditors = Dict.empty
    }


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoReturn )

        SetSearchTerm str ->
            ( { model | searchTerm = str }
            , Cmd.none
            , NoReturn
            )

        SetSorting sorting ->
            ( { model | sorting = sorting }
            , Cmd.none
            , NoReturn
            )

        EditFilter filter ->
            let
                ( filterEditorModel, filterEditorCmd ) =
                    FilterEditor.init filter

                filterHandle =
                    Filter.handle filter
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
            , MapQuery <|
                Query.mapFilters <|
                    Filters.remove <|
                        Filter.handle filter
            )

        Submit ->
            let
                query =
                    if model.searchTerm == "" then
                        Query.OnFolder
                            { folder = Query.getFolder context.query
                            , filters = Query.getFilters context.query
                            }

                    else
                        Query.OnFts
                            { folder = Query.getFolder context.query
                            , filters = Query.getFilters context.query
                            , searchTerm = model.searchTerm
                            , sorting = model.sorting
                            }
            in
            ( model
            , Cmd.none
            , MapQuery (always query)
            )

        SubmitExampleQuery ->
            let
                searchTerm =
                    "variable"

                query =
                    Query.OnFts
                        { folder = Query.getFolder context.query
                        , filters =
                            Filters.none
                                |> Filters.insert (FilterYearWithin "2000" "2010")
                                |> Filters.insert (FilterTitleFts "with")
                        , searchTerm = searchTerm
                        , sorting = model.sorting
                        }
            in
            ( { model | searchTerm = searchTerm }
            , Cmd.none
            , MapQuery (always query)
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
                            , MapQuery
                                (Query.mapFilters
                                    (Filters.remove filterHandle
                                        >> Filters.insert newFilter
                                    )
                                )
                            )

                        FilterEditor.Removed ->
                            ( modelWithEditorClosed
                            , cmd
                            , MapQuery
                                (Query.mapFilters
                                    (Filters.remove filterHandle)
                                )
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
        , if Query.showFilters context.query then
            viewFilters context model

          else
            Html.text ""
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
                , Html.Attributes.value model.searchTerm
                , Utils.onChange SetSearchTerm
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.classList [ ( "selected", model.sorting == Query.ByRank ) ]
                , Html.Events.onClick (SetSorting Query.ByRank)
                ]
                [ Icons.search, Html.text " By Rank" ]
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.classList [ ( "selected", model.sorting == Query.ByDate ) ]
                , Html.Events.onClick (SetSorting Query.ByDate)
                ]
                [ Icons.search, Html.text " By Date" ]
            ]
        ]


viewFilters : Context -> Model -> Html Msg
viewFilters context model =
    Html.div [ Html.Attributes.class "filters-bar" ]
        [ Html.span [] <|
            List.map
                (\{ name, initFilter } ->
                    Html.span
                        [ Html.Attributes.class "input-group" ]
                        [ Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Events.onClick <| EditFilter initFilter
                            , Html.Attributes.class "filter-button"
                            ]
                            [ Html.text <| name ++ "..." ]
                        ]
                )
                Filter.filterTypes
        , viewExistingFilters
            model
            (Query.getFilters context.query)
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
                            (Filter.handle filter)
                            model.filterEditors
                in
                viewExistingFilter beingEdited filter
            )
            (Filters.toList filters)


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
