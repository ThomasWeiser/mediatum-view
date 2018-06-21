module Search
    exposing
        ( Model
        , Specification
        , SearchType(..)
        , SimpleSearchDomain(..)
        , Msg
        , init
        , update
        , view
        , searchTypeText
        )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Pagination
import Page exposing (Page, PageResult)
import Document exposing (Document, Attribute)
import Api


type alias Model =
    { specification : Specification
    , pageResult : PageResult Document
    }


type alias Specification =
    { searchType : SearchType
    , searchString : String
    }


type SearchType
    = SimpleSearch SimpleSearchDomain
    | AuthorSearch


type SimpleSearchDomain
    = SearchAttributes
    | SearchFulltext
    | SearchAll


type Msg
    = ApiResponse (Api.Response (Page Document))
    | PickPosition Pagination.Position


init : Specification -> ( Model, Cmd Msg )
init specification =
    let
        model =
            { specification = specification
            , pageResult = Page.initialPageResult
            }
    in
        update
            (PickPosition Pagination.First)
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickPosition position ->
            sendSearchQuery position model

        ApiResponse result ->
            ( { model
                | pageResult = Page.updatePageResultFromResult result model.pageResult
              }
            , Cmd.none
            )


sendSearchQuery : Pagination.Position -> Model -> ( Model, Cmd Msg )
sendSearchQuery paginationPosition model =
    if model.specification.searchString == "" then
        ( model, Cmd.none )
    else
        ( { model
            | pageResult = Page.loadingPageResult model.pageResult
          }
        , case model.specification.searchType of
            SimpleSearch simpleSearchDomain ->
                Api.makeRequest
                    ApiResponse
                    (Api.querySimpleSearch
                        model.pageResult.page
                        paginationPosition
                        model.specification.searchString
                        (case simpleSearchDomain of
                            SearchAttributes ->
                                [ "attrs" ]

                            SearchFulltext ->
                                [ "fulltext" ]

                            SearchAll ->
                                [ "attrs", "fulltext" ]
                        )
                    )

            AuthorSearch ->
                Api.makeRequest
                    ApiResponse
                    (Api.queryAuthorSearch
                        model.pageResult.page
                        paginationPosition
                        model.specification.searchString
                    )
        )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.span [] [ Html.text "Search " ]
            , Html.span [] [ Html.text <| searchTypeText model.specification.searchType ]
            , Html.span [] [ Html.text ": " ]
            , Html.span [] [ Html.text model.specification.searchString ]
            ]
        , Html.div []
            [ Html.text <| "Loading: "
            , Html.text <| toString model.pageResult.loading
            , Html.text <| ". Error: "
            , Html.text <| toString model.pageResult.error
            ]
        , case model.pageResult.page of
            Nothing ->
                Html.text "No query"

            Just documentPage ->
                viewResponse
                    PickPosition
                    (viewPage Document.view)
                    documentPage
        ]


searchTypeText : SearchType -> String
searchTypeText searchType =
    case searchType of
        SimpleSearch SearchAttributes ->
            "All Attributes"

        SimpleSearch SearchFulltext ->
            "Fulltext"

        SimpleSearch SearchAll ->
            "Attributes and Fulltext"

        AuthorSearch ->
            "Author Surname"


viewResponse :
    (Pagination.Position -> Msg)
    -> (Page itemModel -> Html Msg)
    -> Page itemModel
    -> Html Msg
viewResponse paginationTargetTagger viewEntity page =
    Html.div []
        [ viewNumberOfResults page
        , viewEntity page
        , viewPaginationButtons page paginationTargetTagger
        ]


viewPage : (itemModel -> Html Msg) -> Page itemModel -> Html Msg
viewPage viewItem page =
    Html.div []
        [ Html.div []
            (List.map viewItem (Page.entries page))
        ]


viewNumberOfResults : Page itemModel -> Html msg
viewNumberOfResults page =
    Html.div []
        [ Html.strong
            []
            [ Html.text "Number of Results: "
            , Html.text <|
                -- TODO: Following code is only valid for a simpleSearch
                -- (i.e. with a limit)
                if page.totalCount == Api.sizeLimitSimpleSearch then
                    ">= "
                else
                    ""
            , Html.text <| toString page.totalCount
            ]
        ]


viewPaginationButtons : Page itemModel -> (Pagination.Position -> Msg) -> Html Msg
viewPaginationButtons page targetTagger =
    let
        viewButton : String -> Msg -> Bool -> Html Msg
        viewButton label msg enabled =
            Html.button
                [ Html.Attributes.disabled (not enabled)
                , Html.Events.onClick msg
                ]
                [ Html.text label ]
    in
        Html.div
            [ Html.Attributes.style
                [ ( "margin", "4px 0px 8px 0px" ) ]
            ]
            [ -- "Last" currently does not work with our GraphQL API
              -- Only show "First" and "Next" for now.
              -- May implement a form of infinite scrolling later.
              viewButton "First"
                (targetTagger Pagination.First)
                page.pageInfo.hasPreviousPage

            {-
               , viewButton "Prev"
                   (targetTagger Pagination.Previous)
                   page.pageInfo.hasPreviousPage
            -}
            , viewButton "Next"
                (targetTagger Pagination.Next)
                page.pageInfo.hasNextPage

            {-
               , viewButton "Last"
                   (targetTagger Pagination.Last)
                   page.pageInfo.hasNextPage
            -}
            ]
