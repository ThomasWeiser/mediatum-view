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

import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import List.Extra
import Maybe.Extra
import Query exposing (Query)
import Query.Filter as Filter exposing (Filter)
import Query.FilterEditor as FilterEditor
import Query.Filters as Filters exposing (Filters)
import Tree
import Utils


type alias Context =
    { query : Query
    }


type Return
    = NoReturn
    | MapQuery (Query -> Query)


type alias Model =
    { searchOptions : Query.FtsOptions
    , searchTerm : String
    , filterEditor : Maybe ( String, FilterEditor.Model )
    }


type Msg
    = NoOp
    | SetSearchTerm String
    | SetSearchOptions Query.FtsOptions
    | EditFilter Filter
    | RemoveFilter Filter
    | Submit
    | FilterEditorMsg FilterEditor.Msg
    | SubmitExampleQuery


submitExampleQuery : Msg
submitExampleQuery =
    SubmitExampleQuery


init : () -> Model
init _ =
    { searchOptions = Query.FtsOptions Query.SearchFulltext Query.English
    , searchTerm = ""
    , filterEditor = Nothing
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

        SetSearchOptions searchOptions ->
            ( { model | searchOptions = searchOptions }
            , Cmd.none
            , NoReturn
            )

        EditFilter filter ->
            let
                ( filterEditorModel, filterEditorCmd ) =
                    FilterEditor.init filter
            in
            ( { model
                | filterEditor =
                    Just ( Filter.key filter, filterEditorModel )
              }
            , filterEditorCmd |> Cmd.map (always NoOp)
            , NoReturn
            )

        RemoveFilter filter ->
            ( model
            , Cmd.none
            , MapQuery <|
                Query.mapFilters <|
                    Filters.remove <|
                        Filter.key filter
            )

        Submit ->
            let
                query =
                    if model.searchTerm == "" then
                        Query.OnFolder
                            { folder = Query.getFolder context.query
                            , filters =
                                Query.getFilters context.query
                                    |> Maybe.withDefault Filters.none
                            }

                    else
                        Query.OnFts
                            { folder = Query.getFolder context.query
                            , filters =
                                Query.getFilters context.query
                                    |> Maybe.withDefault Filters.none
                            , options = model.searchOptions
                            , searchTerm = model.searchTerm
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
                                |> Filters.insert (Filter.YearWithin "2000" "2010")
                                |> Filters.insert (Filter.TitleFts "the")
                        , options = model.searchOptions
                        , searchTerm = searchTerm
                        }
            in
            ( { model | searchTerm = searchTerm }
            , Cmd.none
            , MapQuery (always query)
            )

        FilterEditorMsg subMsg ->
            case model.filterEditor of
                Just ( filterKey, filterEditor ) ->
                    let
                        ( subModel, subReturn ) =
                            FilterEditor.update subMsg filterEditor
                    in
                    case subReturn of
                        FilterEditor.NoReturn ->
                            ( { model
                                | filterEditor = Just ( filterKey, subModel )
                              }
                            , Cmd.none
                            , NoReturn
                            )

                        FilterEditor.Saved newFilter ->
                            ( { model | filterEditor = Nothing }
                            , Cmd.none
                            , MapQuery
                                (Query.mapFilters
                                    (Filters.remove filterKey
                                        >> Filters.insert newFilter
                                    )
                                )
                            )

                        FilterEditor.Removed ->
                            ( { model | filterEditor = Nothing }
                            , Cmd.none
                            , MapQuery
                                (Query.mapFilters
                                    (Filters.remove filterKey)
                                )
                            )

                        FilterEditor.Canceled ->
                            ( { model | filterEditor = Nothing }
                            , Cmd.none
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
        [ Html.div [ Html.Attributes.class "search-bar" ]
            [ Html.select
                [ Html.Events.onInput
                    (Query.ftsOptionsFromLabel
                        >> Maybe.Extra.unwrap NoOp SetSearchOptions
                    )
                ]
                (List.map
                    (\ftsOptions ->
                        Html.option
                            [ Html.Attributes.value
                                (Query.ftsOptionsToLabel ftsOptions)
                            , Html.Attributes.selected
                                (model.searchOptions == ftsOptions)
                            ]
                            [ Html.text
                                (Query.ftsOptionsToLabel ftsOptions)
                            ]
                    )
                    [ Query.FtsOptions Query.SearchAttributes Query.English
                    , Query.FtsOptions Query.SearchAttributes Query.German
                    , Query.FtsOptions Query.SearchFulltext Query.English
                    , Query.FtsOptions Query.SearchFulltext Query.German
                    ]
                )
            , Html.input
                [ Html.Attributes.class "search-input"
                , Html.Attributes.type_ "search"
                , Html.Attributes.placeholder "Search ..."
                , Html.Attributes.value model.searchTerm
                , Utils.onChange SetSearchTerm
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit" ]
                [ Icons.search ]
            ]
        ]


viewFilters : Context -> Model -> Html Msg
viewFilters context model =
    Html.div []
        [ Html.div []
            [ Maybe.Extra.unwrap
                (Html.text "")
                (viewExistingFilters model)
                (Query.getFilters context.query)
            ]
        , case model.filterEditor of
            Nothing ->
                Html.span [] <|
                    List.map
                        (\{ name, initFilter } ->
                            Html.button
                                [ Html.Attributes.type_ "button"
                                , Html.Events.onClick <| EditFilter initFilter
                                ]
                                [ Html.text name ]
                        )
                        Filter.filterTypes

            Just ( _, filterEditor ) ->
                FilterEditor.view filterEditor
                    |> Html.map FilterEditorMsg
        ]


viewExistingFilters : Model -> Filters -> Html Msg
viewExistingFilters model filters =
    Html.div [] <|
        List.map
            (\filter ->
                let
                    beingEdited =
                        case model.filterEditor of
                            Just ( filterKey, _ ) ->
                                Filter.key filter == filterKey

                            _ ->
                                False
                in
                viewExistingFilter beingEdited filter
            )
            (Filters.toList filters)


viewExistingFilter : Bool -> Filter -> Html Msg
viewExistingFilter beingEdited filter =
    Html.span
        [ Html.Attributes.class "existing-filter"
        , Html.Attributes.classList [ ( "being-edited", beingEdited ) ]
        ]
        [ Filter.view filter
            |> Html.map never
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.disabled beingEdited
            , Html.Events.onClick (EditFilter filter)
            ]
            [ Html.text "Edit" ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.disabled beingEdited
            , Html.Events.onClick (RemoveFilter filter)
            ]
            [ Html.text "Remove" ]
        ]
