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
import Graphqelm.Extra
import Document exposing (Document, Attribute)
import Api
import Folder exposing (FolderId)
import Tree
import Icons
import Utils


type alias Model =
    { specification : Specification
    , pageResult : PageResult Document
    }


type alias Specification =
    { folder : Maybe FolderId
    , searchType : SearchType
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
    case model.specification.folder of
        Nothing ->
            ( model
            , Cmd.none
            )

        Just folderId ->
            if model.specification.searchString == "" then
                ( { model
                    | pageResult = Page.loadingPageResult model.pageResult
                  }
                , Api.makeRequest
                    ApiResponse
                    (Api.queryFolderDocuments
                        model.pageResult.page
                        paginationPosition
                        folderId
                    )
                )
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
                                folderId
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
                                folderId
                                model.specification.searchString
                            )
                )


view : Tree.Model -> Model -> Html Msg
view tree model =
    Html.div []
        [ Html.div [ Html.Attributes.class "breadcrumbs" ] <|
            case model.specification.folder of
                Just folderId ->
                    [ Tree.viewBreadcrumbs tree folderId ]

                Nothing ->
                    [ Html.text Utils.noBreakSpace ]
        , Html.div [] <|
            if model.specification.searchString == "" then
                [ Html.span [] [ Html.text "All Documents" ] ]
            else
                [ Html.span [] [ Html.text "Search " ]
                , Html.span [] [ Html.text <| searchTypeText model.specification.searchType ]
                , Html.span [] [ Html.text ": \"" ]
                , Html.span [] [ Html.text model.specification.searchString ]
                , Html.span [] [ Html.text "\"" ]
                ]
        , case model.pageResult.page of
            Nothing ->
                Html.text ""

            Just documentPage ->
                viewResponse
                    PickPosition
                    (viewPage Document.view)
                    documentPage
        , if model.pageResult.loading then
            Icons.spinner
          else
            Html.text ""
        , case model.pageResult.error of
            Nothing ->
                Html.text ""

            Just error ->
                viewError error
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


viewError : Graphqelm.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (toString error) ]
