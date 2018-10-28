module Controls exposing
    ( Context
    , Model
    , Msg
    , Return(..)
    , init
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
import Query.FilterEditor as FilterEditor
import Query.Filter as Filter exposing (Filter)
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
    , filterEditor : Maybe ( Maybe Filter, FilterEditor.Model )
    }


type Msg
    = NoOp
    | SetSearchTerm String
    | SetSearchOptions Query.FtsOptions
    | EditNewFilter Filter
    | EditExistingFilter Filter
    | RemoveFilter Filter
    | Submit
    | FilterEditorMsg FilterEditor.Msg


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

        EditNewFilter filter ->
            let
                ( filterEditorModel, filterEditorCmd ) =
                    FilterEditor.init filter
            in
            ( { model
                | filterEditor =
                    Just ( Nothing, filterEditorModel )
              }
            , filterEditorCmd |> Cmd.map (always NoOp)
            , NoReturn
            )

        EditExistingFilter filter ->
            let
                ( filterEditorModel, filterEditorCmd ) =
                    FilterEditor.init filter
            in
            ( { model
                | filterEditor =
                    Just ( Just filter, filterEditorModel )
              }
            , filterEditorCmd |> Cmd.map (always NoOp)
            , NoReturn
            )

        RemoveFilter filter ->
            ( model
            , Cmd.none
            , MapQuery
                (Query.mapFilters
                    (Filters.remove filter)
                )
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

        FilterEditorMsg subMsg ->
            case model.filterEditor of
                Just ( maybeOldFilter, filterEditor ) ->
                    let
                        ( subModel, subReturn ) =
                            FilterEditor.update subMsg filterEditor
                    in
                    case subReturn of
                        FilterEditor.NoReturn ->
                            ( { model
                                | filterEditor = Just ( maybeOldFilter, subModel )
                              }
                            , Cmd.none
                            , NoReturn
                            )

                        FilterEditor.Saved newFilter ->
                            ( { model | filterEditor = Nothing }
                            , Cmd.none
                            , MapQuery
                                (Query.mapFilters
                                    (Maybe.Extra.unwrap
                                        identity
                                        Filters.remove
                                        maybeOldFilter
                                        >> Filters.insert newFilter
                                    )
                                )
                            )

                        FilterEditor.Removed ->
                            ( { model | filterEditor = Nothing }
                            , Cmd.none
                            , MapQuery
                                (Query.mapFilters
                                    (Maybe.Extra.unwrap
                                        identity
                                        Filters.remove
                                        maybeOldFilter
                                    )
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
                                , Html.Events.onClick <| EditNewFilter initFilter
                                ]
                                [ Html.text name ]
                        )
                        Filter.filterTypes

            Just ( maybeOldFilter, filterEditor ) ->
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
                            Just ( Just editingFilter, _ ) ->
                                filter == editingFilter

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
            , Html.Events.onClick (EditExistingFilter filter)
            ]
            [ Html.text "Edit" ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.disabled beingEdited
            , Html.Events.onClick (RemoveFilter filter)
            ]
            [ Html.text "Remove" ]
        ]
