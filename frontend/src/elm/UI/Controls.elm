module UI.Controls exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , submitExampleQuery
    , initialModel
    , update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs submitExampleQuery
@docs initialModel
@docs update
@docs view

-}

import Cache exposing (Cache)
import Cache.Derive
import Config
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Maybe.Extra
import RemoteData
import Types.Aspect as Aspect exposing (Aspect)
import Types.FilterList as FilterList exposing (FilterList)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation exposing (Presentation(..))
import Types.Route exposing (Route)
import Types.SearchTerm as SearchTerm
import Types.Selection as Selection exposing (FtsSorting(..))
import UI.Icons
import Utils
import Utils.Html
import Utils.List


{-| -}
type alias Context =
    { route : Route
    , cache : Cache
    , presentation : Presentation
    }


{-| -}
type Return
    = NoReturn
    | Navigate Navigation


{-| -}
type alias Model =
    { ftsTerm : String
    , ftsSorting : FtsSorting
    , ftsFilterLines : List ( Aspect, String )
    }


{-| -}
type Msg
    = SetSearchTerm String
    | ClearSearchTerm
    | SetSorting FtsSorting
    | AddFtsFilter Aspect
    | SetFtsFilterText Aspect String
    | RemoveFtsFilter Aspect
    | Submit
    | SubmitExampleQuery


{-| -}
submitExampleQuery : Msg
submitExampleQuery =
    SubmitExampleQuery


{-| -}
initialModel : Route -> Model
initialModel route =
    { ftsTerm =
        case route.parameters.ftsTerm of
            Nothing ->
                ""

            Just seachTerm ->
                SearchTerm.toString seachTerm
    , ftsSorting = route.parameters.ftsSorting
    , ftsFilterLines =
        route.parameters.ftsFilters
            |> FilterList.toList
            |> List.map (Tuple.mapSecond SearchTerm.toString)
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        SetSearchTerm ftsTerm ->
            ( { model | ftsTerm = ftsTerm }
            , Cmd.none
            , NoReturn
            )

        ClearSearchTerm ->
            ( { model | ftsTerm = "" }
            , Cmd.none
            , NoReturn
            )

        SetSorting ftsSorting ->
            ( { model | ftsSorting = ftsSorting }
            , Cmd.none
            , NoReturn
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

                filterIsInRoute =
                    FilterList.get aspect context.route.parameters.ftsFilters /= Nothing
            in
            ( model1
            , Cmd.none
            , if filterIsInRoute then
                navigate model1

              else
                NoReturn
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
                        | ftsTerm = "variable"
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
            (SearchTerm.fromString model.ftsTerm)
            model.ftsSorting
            (model.ftsFilterLines
                |> List.filterMap
                    (\( aspect, searchText ) ->
                        SearchTerm.fromString searchText
                            |> Maybe.map (Tuple.pair aspect)
                    )
                |> Selection.ftsFiltersFromList
            )
        )


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.nav []
        [ Html.form
            [ Html.Events.onSubmit Submit ]
            [ viewSearch context model
            , viewFtsFilters model
            ]
        ]


viewSearch : Context -> Model -> Html Msg
viewSearch context model =
    Html.div [ Html.Attributes.class "search-bar" ]
        [ Html.span [ Html.Attributes.class "input-group" ]
            [ Html.input
                [ Html.Attributes.class "search-input"
                , Html.Attributes.type_ "search"
                , Html.Attributes.placeholder
                    (getSearchFieldPlaceholder context)
                , Html.Attributes.value model.ftsTerm
                , Html.Events.onInput SetSearchTerm
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "clear-input"
                , Utils.Html.displayNone (model.ftsTerm == "")
                , Html.Events.onClick ClearSearchTerm
                ]
                [ UI.Icons.clear ]
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.classList
                    [ ( "selected"
                      , model.ftsSorting == FtsByRank
                      )
                    ]
                , Html.Events.onClick (SetSorting FtsByRank)
                ]
                [ UI.Icons.search, Html.text " By Rank" ]
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.classList
                    [ ( "selected"
                      , model.ftsSorting == FtsByDate
                      )
                    ]
                , Html.Events.onClick (SetSorting FtsByDate)
                ]
                [ UI.Icons.search, Html.text " By Date" ]
            ]
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


viewFtsFilters : Model -> Html Msg
viewFtsFilters model =
    Html.div [ Html.Attributes.class "filters-bar" ]
        [ viewExistingFtsFilters
            model.ftsFilterLines
        , viewFtsAspectButtons model.ftsFilterLines
        ]


viewExistingFtsFilters : List ( Aspect, String ) -> Html Msg
viewExistingFtsFilters ftsFilterLines =
    Html.div [] <|
        List.map
            (\( aspect, searchText ) ->
                viewExistingFtsFilter aspect searchText
            )
            ftsFilterLines


viewExistingFtsFilter : Aspect -> String -> Html Msg
viewExistingFtsFilter aspect searchText =
    Html.div
        [ Html.Attributes.class "search-bar"

        -- , Html.Attributes.classList [ ( "being-edited", beingEdited ) ]
        ]
        [ Html.label
            [ Html.Attributes.class "search-label" ]
            [ Html.text (Aspect.toString aspect) ]
        , Html.span
            [ Html.Attributes.class "input-group" ]
            [ Html.input
                [ Html.Attributes.class "search-input"
                , Html.Attributes.type_ "search"
                , Html.Attributes.placeholder <| "Search " ++ Aspect.toString aspect
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


viewFtsAspectButtons : List ( Aspect, String ) -> Html Msg
viewFtsAspectButtons ftsFilterLines =
    Html.div [] <|
        List.filterMap
            (\aspect ->
                if Utils.List.findByMapping Tuple.first aspect ftsFilterLines == Nothing then
                    Just <|
                        Html.span
                            [ Html.Attributes.class "" ]
                            [ Html.button
                                [ Html.Attributes.type_ "button"
                                , Html.Attributes.class "add-filter-button"
                                , Html.Events.onClick <| AddFtsFilter aspect
                                ]
                                [ Html.text <| Aspect.toString aspect ]
                            ]

                else
                    Nothing
            )
            Config.validFtsAspects
