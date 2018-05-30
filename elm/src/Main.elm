module Main exposing (main)

import Api.Object
import Api.Object.PageInfo
import Api.Object.DocumentsConnection
import Api.Object.DocumentsEdge
import Api.Object.Document
import Api.Object.Metadatatype
import Api.Query
import Api.Scalar exposing (Cursor)
import Json.Decode exposing (Decoder)
import Regex
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Select
import Graphqelm.Field
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Graphqelm.SelectionSet exposing (SelectionSet, with)
import Graphqelm.Http
import Graphqelm.Operation
import Connection
import Pagination


type SearchType
    = SimpleSearch SimpleSearchDomain
    | AuthorSearch


type SimpleSearchDomain
    = SearchAttributes
    | SearchFulltext
    | SearchAll


type alias Page itemModel =
    Connection.Connection Cursor itemModel


type alias Model =
    { searchType : SearchType
    , searchString : String
    , documents : Maybe (Page Document)
    }


type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }


type alias Document =
    { metadatatypeName : String
    , attributes : List Attribute
    }


type ValueListType
    = ValueListType


pageSize : Int
pageSize =
    10


sizeLimitSimpleSearch : Int
sizeLimitSimpleSearch =
    100


sendSearchQuery : Pagination.Position -> Model -> Cmd Msg
sendSearchQuery paginationPosition model =
    case model.searchType of
        SimpleSearch simpleSearchDomain ->
            makeRequest
                (querySimpleSearch
                    model.documents
                    paginationPosition
                    model.searchString
                    (case simpleSearchDomain of
                        SearchAttributes ->
                            [ "attrs" ]

                        SearchFulltext ->
                            [ "fulltext" ]

                        SearchAll ->
                            [ "attrs", "fulltext" ]
                    )
                )
                requestResultTagger

        AuthorSearch ->
            makeRequest
                (queryAuthorSearch
                    model.documents
                    paginationPosition
                    model.searchString
                )
                requestResultTagger


makeRequest :
    SelectionSet (Page Document) Graphqelm.Operation.RootQuery
    -> (Result (Graphqelm.Http.Error (Page Document)) (Page Document) -> Msg)
    -> Cmd Msg
makeRequest query tagger =
    query
        |> Graphqelm.Http.queryRequest "http://localhost:5000/graphql"
        |> Graphqelm.Http.send tagger


requestResultTagger :
    Result (Graphqelm.Http.Error (Page Document)) (Page Document)
    -> Msg
requestResultTagger httpSendResult =
    case httpSendResult of
        Err error ->
            GraphqelmHttpError error

        Ok documentPage ->
            DocumentsPageResult documentPage


apiDocumentObjects : Connection.ApiObjects {} Api.Object.DocumentsConnection Api.Object.DocumentsEdge Api.Object.Document Api.Object.PageInfo Cursor Document
apiDocumentObjects =
    { connectionSelection = Api.Object.DocumentsConnection.selection
    , totalCount = Api.Object.DocumentsConnection.totalCount
    , pageInfo = Api.Object.DocumentsConnection.pageInfo
    , edges = Api.Object.DocumentsConnection.edges
    , edgeSelection = Api.Object.DocumentsEdge.selection
    , cursor = Api.Object.DocumentsEdge.cursor
    , node = Api.Object.DocumentsEdge.node
    , pageInfoSelection = Api.Object.PageInfo.selection
    , hasNextPage = Api.Object.PageInfo.hasNextPage
    , hasPreviousPage = Api.Object.PageInfo.hasPreviousPage
    }


querySimpleSearch :
    Maybe (Page Document)
    -> Pagination.Position
    -> String
    -> List String
    -> SelectionSet (Page Document) Graphqelm.Operation.RootQuery
querySimpleSearch referencePage paginationPosition searchString searchDomains =
    Api.Query.selection identity
        |> with
            (Api.Query.simpleSearch
                ((\optionals ->
                    { optionals
                        | text = Present searchString
                        , domains = Present (List.map Just searchDomains)
                        , limit = Present sizeLimitSimpleSearch
                    }
                 )
                    >> Pagination.paginationArguments pageSize referencePage paginationPosition
                )
                (Connection.connection
                    apiDocumentObjects
                    documentNode
                )
            )


queryAuthorSearch :
    Maybe (Page Document)
    -> Pagination.Position
    -> String
    -> SelectionSet (Page Document) Graphqelm.Operation.RootQuery
queryAuthorSearch referencePage paginationPosition searchString =
    Api.Query.selection identity
        |> with
            (Api.Query.authorSearch
                ((\optionals ->
                    { optionals
                        | text = Present searchString
                    }
                 )
                    >> Pagination.paginationArguments pageSize referencePage paginationPosition
                )
                (Connection.connection
                    apiDocumentObjects
                    documentNode
                )
            )


documentNode : SelectionSet Document Api.Object.Document
documentNode =
    Api.Object.Document.selection Document
        |> with
            (Api.Object.Document.metadatatype
                (Api.Object.Metadatatype.selection identity
                    |> with
                        (Api.Object.Metadatatype.longname
                            |> Graphqelm.Field.nonNullOrFail
                        )
                )
                |> Graphqelm.Field.nonNullOrFail
            )
        |> with
            (Api.Object.Document.valuesByMask
                (\optionals ->
                    { optionals
                        | maskName = Present "nodesmall"
                    }
                )
                |> Graphqelm.Field.map mapJsonToAttributes
            )


mapJsonToAttributes : Maybe Api.Scalar.Json -> List Attribute
mapJsonToAttributes maybeJson =
    case maybeJson of
        Nothing ->
            []

        Just (Api.Scalar.Json str) ->
            Result.withDefault [] <|
                Json.Decode.decodeString decoderAttributeList str


decoderAttributeList : Decoder (List Attribute)
decoderAttributeList =
    Json.Decode.oneOf
        [ Json.Decode.null []
        , Json.Decode.list <|
            Json.Decode.map4 Attribute
                (Json.Decode.field "field" Json.Decode.string)
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "width" Json.Decode.int)
                (Json.Decode.field "value" (Json.Decode.maybe Json.Decode.string))
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Msg
    = SearchString String
    | SetSearchType SearchType
    | GraphqelmHttpError (Graphqelm.Http.Error (Page Document))
    | DocumentsPageResult (Page Document)
    | PickPosition PageView Pagination.Position


type PageView
    = ViewDocuments


init : ( Model, Cmd Msg )
init =
    let
        model =
            { searchType = SimpleSearch SearchAttributes
            , searchString = ""
            , documents = Nothing
            }
    in
        ( model
        , sendSearchQuery Pagination.First model
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchString str ->
            let
                model1 =
                    { model | searchString = str }
            in
                ( model1
                , sendSearchQuery Pagination.First model1
                )

        SetSearchType searchType ->
            let
                model1 =
                    { model | searchType = searchType }
            in
                ( model1
                , sendSearchQuery Pagination.First model1
                )

        PickPosition pageView position ->
            ( model
            , case pageView of
                ViewDocuments ->
                    sendSearchQuery position model
            )

        GraphqelmHttpError graphqelmHttpError ->
            let
                _ =
                    Debug.log "... Graphqelm Http Error" graphqelmHttpError
            in
                ( model, Cmd.none )

        DocumentsPageResult documents ->
            ( { model | documents = Just documents }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h2 []
            [ Html.text "mediaTUM HSB Demo 2018-05-30"
            , Html.div [ Html.Attributes.class "color" ]
                [ Html.text "PostgreSQL · PostGraphQL · GraphQL · Elm" ]
            ]
        , Html.hr [] []
        , viewSearchControls model
        , Html.hr [] []
        , model.documents
            |> viewResponse (PickPosition ViewDocuments) (viewPage viewDocument)
        , Html.hr [] []
        ]


viewSearchTypeLabel : SearchType -> String
viewSearchTypeLabel searchType =
    case searchType of
        SimpleSearch SearchAttributes ->
            "All Attributes"

        SimpleSearch SearchFulltext ->
            "Fulltext"

        SimpleSearch SearchAll ->
            "Attributes and Fulltext"

        AuthorSearch ->
            "Author Surname"


viewSearchControls : Model -> Html Msg
viewSearchControls model =
    Html.div []
        [ Html.input
            [ Html.Attributes.class "searchInput"
            , Html.Attributes.placeholder "Search ..."
            , Html.Attributes.value model.searchString
            , Html.Events.onInput SearchString
            ]
            []
        , Select.fromSelected_
            [ SimpleSearch SearchAttributes, SimpleSearch SearchFulltext, SimpleSearch SearchAll, AuthorSearch ]
            SetSearchType
            toString
            viewSearchTypeLabel
            model.searchType
        ]


viewResponse :
    (Pagination.Position -> Msg)
    -> (Page itemModel -> Html Msg)
    -> Maybe (Page itemModel)
    -> Html Msg
viewResponse paginationTargetTagger viewEntity response =
    Html.div []
        [ viewNumberOfResults response
        , case response of
            Nothing ->
                Html.div [] [ Html.text "[...]" ]

            Just page ->
                viewEntity page
        , viewPaginationButtons response paginationTargetTagger
        ]


viewPage : (itemModel -> Html Msg) -> Page itemModel -> Html Msg
viewPage viewItem page =
    Html.div []
        [ Html.div []
            (List.map viewItem (Connection.nodes page))
        ]


viewNumberOfResults : Maybe (Page itemModel) -> Html msg
viewNumberOfResults response =
    Html.div []
        [ Html.strong
            []
            (Html.text "Number of Results: "
                :: case response of
                    Nothing ->
                        [ Html.text "[...]" ]

                    Just page ->
                        [ Html.text <|
                            -- TODO: Following code is only valid for a simpleSearch (i.e. with a limit)
                            if page.totalCount == sizeLimitSimpleSearch then
                                ">= "
                            else
                                ""
                        , Html.text <| toString page.totalCount
                        ]
            )
        ]


viewPaginationButtons : Maybe (Page itemModel) -> (Pagination.Position -> Msg) -> Html Msg
viewPaginationButtons response targetTagger =
    let
        ( hasPreviousPage, hasNextPage ) =
            case response of
                Just { pageInfo } ->
                    ( pageInfo.hasPreviousPage, pageInfo.hasNextPage )

                Nothing ->
                    ( False, False )

        viewButton : Bool -> String -> Msg -> Html Msg
        viewButton enabled label msg =
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
              viewButton hasPreviousPage "First" (targetTagger Pagination.First)

            -- viewButton hasPreviousPage "Prev" (targetTagger Previous),
            , viewButton hasNextPage "Next" (targetTagger Pagination.Next)

            -- , viewButton hasNextPage "Last" (targetTagger Last)
            ]


viewDocument : Document -> Html Msg
viewDocument document =
    Html.div [ Html.Attributes.class "document" ]
        [ Html.div [ Html.Attributes.class "metadatatype" ]
            [ Html.text document.metadatatypeName ]
        , Html.div [ Html.Attributes.class "attributes" ]
            (List.map
                viewAttribute
                document.attributes
            )
        ]


maxAttributeStringLength : Int
maxAttributeStringLength =
    80


viewAttribute : Attribute -> Html Msg
viewAttribute attribute =
    let
        isField regex =
            Regex.contains (Regex.regex regex) attribute.field
    in
        (case attribute.value of
            Just valueLong ->
                let
                    value =
                        if String.length valueLong > maxAttributeStringLength then
                            String.left (maxAttributeStringLength - 3) valueLong ++ "..."
                        else
                            valueLong
                in
                    Html.span
                        [ Html.Attributes.classList
                            [ ( "attribute", True )
                            , ( "author", isField "author" )
                            , ( "title"
                              , isField "title"
                                    && not (isField "congress|journal")
                              )
                            ]
                        , Html.Attributes.title (attribute.name ++ ": " ++ valueLong)
                        ]
                        [ Html.text <|
                            if isField "year" then
                                String.left 4 value ++ ". "
                            else if isField "author" then
                                value ++ ": "
                            else if isField "title|type" then
                                value ++ ". "
                            else
                                attribute.name ++ ": " ++ value ++ ". "
                        ]

            Nothing ->
                Html.text ""
        )
