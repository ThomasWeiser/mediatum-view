module Article.Fts
    exposing
        ( Model
        , Specification
        , SearchType(..)
        , FtsSearchDomain(..)
        , FtsSearchLanguage(..)
        , Msg
        , Return(..)
        , init
        , update
        , view
        , searchTypeText
        )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Pagination.Offset.Page as Page exposing (Page, PageResult)
import Graphqelm.Extra
import FtsDocumentResult exposing (FtsDocumentResult)
import Document exposing (Document, DocumentId)
import Api
import Folder exposing (Folder, FolderCountMap)
import Icons
import Utils


type alias Context =
    { folder : Folder
    }


type alias Model =
    { specification : Specification
    , pageResult : PageResult FtsDocumentResult
    }


type alias Specification =
    { searchType : SearchType
    , searchString : String
    }


type SearchType
    = FtsSearch FtsSearchDomain FtsSearchLanguage


type FtsSearchDomain
    = SearchAttributes
    | SearchFulltext


type FtsSearchLanguage
    = English
    | German


type Msg
    = ApiResponseFtsPage (Api.Response (Page FtsDocumentResult))
    | ApiResponseFtsFolderCounts (Api.Response FolderCountMap)
    | PickPosition Page.Position
    | SelectDocument DocumentId


type Return
    = NoReturn
    | SelectedDocument DocumentId
    | FolderCounts FolderCountMap


init : Context -> Specification -> ( Model, Cmd Msg )
init context specification =
    let
        model =
            { specification = specification
            , pageResult = Page.initialPageResult
            }
    in
        update
            (PickPosition Page.First)
            context
            model
            |> Utils.tupleRemoveThird


update : Msg -> Context -> Model -> ( Model, Cmd Msg, Return )
update msg context model =
    case msg of
        PickPosition position ->
            sendSearchQuery position context model
                |> Utils.tupleAddThird NoReturn

        ApiResponseFtsPage result ->
            ( { model
                | pageResult = Page.updatePageResultFromResult result model.pageResult
              }
            , case model.specification.searchType of
                FtsSearch ftsSearchDomain ftsSearchLanguage ->
                    Api.makeRequest
                        ApiResponseFtsFolderCounts
                        (Api.queryFtsFolderCounts
                            context.folder.id
                            model.specification.searchString
                            (case ftsSearchDomain of
                                SearchAttributes ->
                                    "attrs"

                                SearchFulltext ->
                                    "fulltext"
                            )
                            (case ftsSearchLanguage of
                                English ->
                                    "english"

                                German ->
                                    "german"
                            )
                        )
            , NoReturn
            )

        ApiResponseFtsFolderCounts (Err error) ->
            -- TODO
            ( model
            , Cmd.none
            , NoReturn
            )

        ApiResponseFtsFolderCounts (Ok folderCountMap) ->
            ( model
            , Cmd.none
            , FolderCounts folderCountMap
            )

        SelectDocument id ->
            ( model, Cmd.none, SelectedDocument id )


sendSearchQuery : Page.Position -> Context -> Model -> ( Model, Cmd Msg )
sendSearchQuery paginationPosition context model =
    ( { model
        | pageResult = Page.loadingPageResult model.pageResult
      }
    , case model.specification.searchType of
        FtsSearch ftsSearchDomain ftsSearchLanguage ->
            Api.makeRequest
                ApiResponseFtsPage
                (Api.queryFtsPage
                    model.pageResult.page
                    paginationPosition
                    context.folder.id
                    model.specification.searchString
                    (case ftsSearchDomain of
                        SearchAttributes ->
                            "attrs"

                        SearchFulltext ->
                            "fulltext"
                    )
                    (case ftsSearchLanguage of
                        English ->
                            "english"

                        German ->
                            "german"
                    )
                )
      {-
         AuthorSearch ->
             Api.makeRequest
                 ApiResponseFtsPage
                 (Api.queryAuthorSearch
                     model.pageResult.page
                     paginationPosition
                     context.folder.id
                     model.specification.searchString
                 )
      -}
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] <|
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
                    (viewPage (FtsDocumentResult.view SelectDocument))
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
        FtsSearch SearchAttributes English ->
            "All Attributes - English"

        FtsSearch SearchAttributes German ->
            "All Attributes - German"

        FtsSearch SearchFulltext English ->
            "Fulltext - English"

        FtsSearch SearchFulltext German ->
            "Fulltext - German"


viewResponse :
    (Page.Position -> Msg)
    -> (Page itemModel -> Html Msg)
    -> Page itemModel
    -> Html Msg
viewResponse paginationTargetTagger viewEntity page =
    Html.div []
        [ -- viewNumberOfResults page,
          viewEntity page
        , viewPaginationButtons page paginationTargetTagger
        ]


viewPage : (itemModel -> Html Msg) -> Page itemModel -> Html Msg
viewPage viewItem page =
    Html.div []
        [ Html.div []
            (List.map viewItem (Page.entries page))
        ]


viewPaginationButtons : Page itemModel -> (Page.Position -> Msg) -> Html Msg
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
            [ viewButton "First"
                (targetTagger Page.First)
                (not (Page.isFirstPage page))
            , viewButton "Prev"
                (targetTagger Page.Previous)
                (not (Page.isFirstPage page))
            , viewButton "Next"
                (targetTagger Page.Next)
                page.hasNextPage
            ]


viewError : Graphqelm.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (toString error) ]
