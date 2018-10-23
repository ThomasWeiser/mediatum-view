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
import Maybe.Extra
import Query exposing (Query)
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
    }


type Msg
    = NoOp
    | SetSearchTerm String
    | SetSearchOptions Query.FtsOptions
    | Submit


init : () -> Model
init _ =
    { searchOptions = Query.FtsOptions Query.SearchFulltext Query.English
    , searchTerm = ""
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


view : Context -> Model -> Html Msg
view { query } model =
    Html.form
        [ Html.Attributes.class "search-bar"
        , Html.Events.onSubmit Submit
        ]
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
