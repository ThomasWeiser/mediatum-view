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
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import RemoteData
import Sort.Dict
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation exposing (Presentation(..))
import Types.Range as Range
import Types.Route exposing (Route)
import Types.Route.Filter
import Types.SearchTerm as SearchTerm
import Types.Selection as Selection exposing (Filter(..), FilterHandle, FtsSorting(..), SetOfFilters)
import UI.Controls.Filter
import UI.Controls.FilterEditor as FilterEditor
import UI.Icons
import Utils


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
    , filterEditors : Sort.Dict.Dict FilterHandle FilterEditor.Model
    }


{-| -}
type Msg
    = SetSearchTerm String
    | SetSorting FtsSorting
    | AddFilter UI.Controls.Filter.FilterType
    | EditFilter Filter
    | RemoveFilter Filter
    | Submit
    | SubmitExampleQuery
    | FilterEditorMsg FilterHandle FilterEditor.Msg


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
    , filterEditors = Sort.Dict.empty (Utils.sorter Selection.orderingFilterHandle)
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    let
        removeFilter filterHandle =
            Types.Route.Filter.fromRoute context.route
                |> Selection.removeFilter filterHandle
                |> Navigation.ShowListingWithFilters
                |> Navigate

        insertFilter oldFilterHandlefilter newFilter =
            Types.Route.Filter.fromRoute context.route
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

                newFilterHandle =
                    Selection.newFilterHandle filterType.name
            in
            ( { model
                | filterEditors =
                    Sort.Dict.insert newFilterHandle filterEditorModel model.filterEditors
              }
            , filterEditorCmd |> Cmd.map (FilterEditorMsg newFilterHandle)
            , NoReturn
            )

        EditFilter filter ->
            let
                ( filterEditorModel, filterEditorCmd ) =
                    FilterEditor.init (UI.Controls.Filter.controlsFromFilter filter)

                filterHandle =
                    Selection.filterHandle filter
            in
            ( { model
                | filterEditors =
                    Sort.Dict.insert filterHandle filterEditorModel model.filterEditors
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
            case Sort.Dict.get filterHandle model.filterEditors of
                Just filterEditor ->
                    let
                        ( subModel, subCmd, subReturn ) =
                            FilterEditor.update subMsg filterEditor

                        modelWithEditorClosed =
                            { model
                                | filterEditors =
                                    Sort.Dict.remove filterHandle model.filterEditors
                            }

                        cmd =
                            subCmd |> Cmd.map (FilterEditorMsg filterHandle)
                    in
                    case subReturn of
                        FilterEditor.NoReturn ->
                            ( { model
                                | filterEditors =
                                    Sort.Dict.insert filterHandle subModel model.filterEditors
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


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.nav []
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
                , Html.Attributes.placeholder
                    (getSearchFieldPlaceholder context)
                , Html.Attributes.value model.ftsTerm
                , Html.Events.onInput SetSearchTerm
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
                UI.Controls.Filter.filterTypes
        , viewExistingFilters
            model
            (Types.Route.Filter.fromRoute context.route)
        , Html.span [] <|
            List.map
                (\( filterHandle, filterEditor ) ->
                    FilterEditor.view filterEditor
                        |> Html.map (FilterEditorMsg filterHandle)
                )
                (Sort.Dict.toList model.filterEditors)
        ]


viewExistingFilters : Model -> SetOfFilters -> Html Msg
viewExistingFilters model filters =
    Html.span [] <|
        List.map
            (\filter ->
                let
                    beingEdited =
                        Sort.Dict.memberOf
                            model.filterEditors
                            (Selection.filterHandle filter)
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
            (UI.Controls.Filter.viewFilterDescription filter)
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.disabled beingEdited
            , Html.Events.onClick (RemoveFilter filter)
            , Html.Attributes.class "filter-button"
            ]
            [ Html.text "X" ]
        ]
