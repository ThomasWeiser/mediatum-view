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
import Query.EditFilter as EditFilter
import Query.Filter as Filter exposing (Filter)
import Query.Filters as Filters exposing (Filters)
import Tree


type alias Context =
    { query : Query
    }


type Return
    = NoReturn
    | MapQuery (Query -> Query)


type alias Model =
    { searchOptions : Query.FtsOptions
    , searchTerm : String
    , editFilter : Maybe EditFilter.Model
    }


type Msg
    = NoOp
    | SetSearchTerm String
    | SetSearchOptions Query.FtsOptions
    | EditNewFilter
    | RemoveFilter Filter
    | Submit
    | EditFilterMsg EditFilter.Msg


init : () -> Model
init _ =
    { searchOptions = Query.FtsOptions Query.SearchFulltext Query.English
    , searchTerm = ""
    , editFilter = Nothing
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

        EditNewFilter ->
            ( { model | editFilter = Just EditFilter.init }
            , Cmd.none
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
                            , filters = Query.exampleFilters
                            }

                    else
                        Query.OnFts
                            { folder = Query.getFolder context.query
                            , filters = Query.exampleFilters
                            , options = model.searchOptions
                            , searchTerm = model.searchTerm
                            }
            in
            ( model
            , Cmd.none
            , MapQuery (always query)
            )

        EditFilterMsg subMsg ->
            case model.editFilter of
                Just editFilter ->
                    let
                        ( subModel, subReturn ) =
                            EditFilter.update subMsg editFilter
                    in
                    case subReturn of
                        EditFilter.NoReturn ->
                            ( { model | editFilter = Just subModel }
                            , Cmd.none
                            , NoReturn
                            )

                        EditFilter.NewFilter newFilter ->
                            ( { model | editFilter = Nothing }
                            , Cmd.none
                            , MapQuery
                                (Query.mapFilters
                                    (Filters.insert newFilter)
                                )
                            )

                Nothing ->
                    ( model, Cmd.none, NoReturn )


view : Context -> Model -> Html Msg
view { query } model =
    Html.form
        [ Html.Events.onSubmit Submit
        ]
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
                , Html.Events.onInput SetSearchTerm
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.value "Search"
                ]
                [ Icons.search ]
            ]
        , Html.div []
            [ Maybe.Extra.unwrap
                (Html.text "")
                viewFilters
                (Query.getFilters query)
            ]
        , case model.editFilter of
            Nothing ->
                Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick EditNewFilter
                    ]
                    [ Html.text "Year" ]

            Just editFilter ->
                EditFilter.view editFilter
                    |> Html.map EditFilterMsg
        ]


viewFilters : Filters -> Html Msg
viewFilters filters =
    Html.div [] <|
        List.map
            (\filter ->
                Filter.view filter
                    |> Html.map
                        (\filterMsg ->
                            case filterMsg of
                                Filter.Remove ->
                                    RemoveFilter filter
                        )
            )
            (Filters.toList filters)
