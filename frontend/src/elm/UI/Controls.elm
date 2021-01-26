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
import Types.Aspect as Aspect exposing (Aspect)
import Types.Navigation as Navigation exposing (Navigation)
import Types.Presentation exposing (Presentation(..))
import Types.Range as Range
import Types.Route exposing (Route)
import Types.SearchTerm as SearchTerm exposing (SearchTerm)
import Types.Selection as Selection exposing (FtsFilters, FtsSorting(..))
import UI.Icons
import Utils
import Utils.Html


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
    }


{-| -}
type Msg
    = SetSearchTerm String
    | ClearSearchTerm
    | SetSorting FtsSorting
      -- | AddFtsFilter Aspect
      -- | EditFtsFilter Aspect SearchTerm
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
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    let
        insertFilter aspect searchTerm =
            Navigation.ShowListingWithAddedFtsFilter aspect searchTerm
                |> Navigate
    in
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

        {- AddFtsFilter aspect ->
               let
                   ( filterEditorModel, filterEditorCmd ) =
                       FilterEditor.init aspect
               in
               ( { model
                   | filterEditors =
                       Sort.Dict.insert aspect filterEditorModel model.filterEditors
                 }
               , filterEditorCmd |> Cmd.map (FilterEditorMsg aspect)
               , NoReturn
               )

           EditFtsFilter aspect searchTerm ->
               let
                   ( filterEditorModel, filterEditorCmd ) =
                       FilterEditor.init (UI.Controls.Filter.controlsFromFilter filter)
               in
               ( { model
                   | filterEditors =
                       Sort.Dict.insert aspect filterEditorModel model.filterEditors
                 }
               , filterEditorCmd |> Cmd.map (FilterEditorMsg aspect)
               , NoReturn
               )
        -}
        RemoveFtsFilter aspect ->
            ( model
            , Cmd.none
            , Navigate (Navigation.ShowListingWithRemovedFtsFilter aspect)
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
            ( model
            , Cmd.none
            , [ Navigation.ShowListingWithSearch
                    (SearchTerm.fromString "variable")
                    context.route.parameters.ftsSorting
              , Navigation.ShowListingWithAddedFtsFilter
                    (Aspect.fromString "person")
                    (SearchTerm.fromStringWithDefault "no-default-needed" "Helmut")
              , Navigation.ShowListingWithAddedFtsFilter
                    (Aspect.fromString "title")
                    (SearchTerm.fromStringWithDefault "no-default-needed" "with")
              ]
                |> Navigation.ListOfNavigations
                |> Navigate
            )


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
        [ Html.div [ Html.Attributes.class "search-bar" ]
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
        [ {- Html.span [] <|
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
                 UI.Controls.Filter.filterTypes ,
          -}
          viewExistingFilters
            model
            context.route.parameters.ftsFilters
        ]


viewExistingFilters : Model -> FtsFilters -> Html Msg
viewExistingFilters model ftsFilters =
    Html.span [] <|
        List.map
            (\( aspect, searchTerm ) ->
                viewExistingFilter aspect searchTerm
            )
            (Sort.Dict.toList ftsFilters)


viewExistingFilter : Aspect -> SearchTerm -> Html Msg
viewExistingFilter aspect searchTerm =
    Html.span
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
                , Html.Attributes.placeholder "Placeholder Todo"
                , Html.Attributes.value (SearchTerm.toString searchTerm)

                -- , Html.Events.onInput SetSearchTerm
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
