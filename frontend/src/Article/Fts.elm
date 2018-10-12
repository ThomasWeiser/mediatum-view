module Article.Fts exposing
    ( FtsSearchDomain(..)
    , FtsSearchLanguage(..)
    , Model
    , Msg
    , Return(..)
    , SearchType(..)
    , Specification
    , init
    , searchTypeFromLabel
    , searchTypeToLabel
    , update
    , view
    )

import Api
import Document exposing (Document, DocumentId)
import Folder exposing (Folder, FolderCounts)
import FtsDocumentResult exposing (FtsDocumentResult)
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Pagination.Offset.Page as Page exposing (Page, PageResult)
import Utils


type alias Context =
    { folder : Folder
    }


type alias Model =
    { specification : Specification
    , pageResult : PageResult FtsDocumentResult
    , queryFolderCounts : Bool
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
    | ApiResponseFtsFolderCounts (Api.Response FolderCounts)
    | PickPosition Page.Position
    | SelectDocument DocumentId


type Return
    = NoReturn
    | SelectedDocument DocumentId
    | FolderCounts FolderCounts


init : Context -> Specification -> ( Model, Cmd Msg )
init context specification =
    let
        model =
            { specification = specification
            , pageResult = Page.initialPageResult
            , queryFolderCounts = True
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
        PickPosition paginationPosition ->
            ( { model
                | pageResult = Page.loadingPageResult model.pageResult
              }
            , Api.makeRequest
                ApiResponseFtsPage
                (Api.queryFtsPage
                    model.pageResult.page
                    paginationPosition
                    context.folder.id
                    model.specification.searchString
                    (searchTypeDomainToString model.specification.searchType)
                    (searchTypeLanguageToString model.specification.searchType)
                )
            , NoReturn
            )

        ApiResponseFtsPage result ->
            ( { model
                | pageResult = Page.updatePageResultFromResult result model.pageResult
                , queryFolderCounts = False
              }
            , if model.queryFolderCounts then
                Api.makeRequest
                    ApiResponseFtsFolderCounts
                    (Api.queryFtsFolderCounts
                        context.folder.id
                        model.specification.searchString
                        (searchTypeDomainToString model.specification.searchType)
                        (searchTypeLanguageToString model.specification.searchType)
                    )

              else
                Cmd.none
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


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] <|
            if model.specification.searchString == "" then
                [ Html.span [] [ Html.text "All Documents" ] ]

            else
                [ Html.span [] [ Html.text "Search " ]
                , Html.span [] [ Html.text <| searchTypeToLabel model.specification.searchType ]
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


searchTypeDomainToString : SearchType -> String
searchTypeDomainToString searchType =
    case searchType of
        FtsSearch SearchAttributes _ ->
            "attrs"

        FtsSearch SearchFulltext _ ->
            "fulltext"


searchTypeLanguageToString : SearchType -> String
searchTypeLanguageToString searchType =
    case searchType of
        FtsSearch _ English ->
            "english"

        FtsSearch _ German ->
            "german"


searchTypeToLabel : SearchType -> String
searchTypeToLabel searchType =
    case searchType of
        FtsSearch SearchAttributes English ->
            "All Attributes - English"

        FtsSearch SearchAttributes German ->
            "All Attributes - German"

        FtsSearch SearchFulltext English ->
            "Fulltext - English"

        FtsSearch SearchFulltext German ->
            "Fulltext - German"


searchTypeFromLabel : String -> Maybe SearchType
searchTypeFromLabel label =
    case label of
        "All Attributes - English" ->
            Just <| FtsSearch SearchAttributes English

        "All Attributes - German" ->
            Just <| FtsSearch SearchAttributes German

        "Fulltext - English" ->
            Just <| FtsSearch SearchFulltext English

        "Fulltext - German" ->
            Just <| FtsSearch SearchFulltext German

        _ ->
            Nothing


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
        [ Html.Attributes.style "margin" "4px 0px 8px 0px"
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


viewError : Graphql.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (toString error) ]
