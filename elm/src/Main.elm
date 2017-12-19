module Main exposing (main)

import Maybe.Extra
import Result.Extra
import Json.Decode exposing (Decoder)
import Regex
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Select
import Task
import GraphQL.Request.Builder as GB
import GraphQL.Request.Builder.Arg as GBArg
import GraphQL.Request.Builder.Variable as GBVar
import GraphQL.Client.Http as GraphQLClient


paginationSize : Int
paginationSize =
    10


type alias Cursor =
    String


type alias Page node =
    { totalCount : Int -- Not in relay spec! Added by PostGraphQL. See https://www.graphile.org/postgraphile/connections/
    , pageInfo : PageInfo
    , edges : List (Edge node)
    }


type alias PageInfo =
    { hasNextPage : Bool
    , hasPreviousPage : Bool
    , startCursor : Maybe Cursor -- Not in relay spec! Added by PostGraphQL
    , endCursor : Maybe Cursor -- Not in relay spec! Added by PostGraphQL
    }


type alias Edge node =
    { cursor : Cursor
    , node : node
    }


type SearchType
    = SimpleSearch SimpleSearchDomain
    | AuthorSearch


type SimpleSearchDomain
    = SearchAttributes
    | SearchFulltext
    | SearchAll


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


valueSpecDocument : GB.ValueSpec GB.NonNull GB.ObjectType Document vars
valueSpecDocument =
    GB.object Document
        |> GB.with
            (GB.field "metadatatype"
                []
                (GB.object identity
                    |> GB.with
                        (GB.field "longname"
                            []
                            GB.string
                        )
                )
            )
        |> GB.with
            (GB.field "valuesByMask"
                [ ( "maskName", GBArg.string "nodesmall" ) ]
                (GB.customScalar ValueListType decoderAttributeList)
            )


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


sizeLimitSimpleSearch : Int
sizeLimitSimpleSearch =
    100


querySimpleSearch :
    Maybe (Page Document)
    -> PaginationTarget
    -> GB.Document GB.Query (Page Document) { vars | searchString : String, searchDomains : List String }
querySimpleSearch prevResponse paginationTarget =
    valueSpecDocument
        |> connectionPage
        |> GB.field "simpleSearch"
            ([ ( "limit", GBArg.int sizeLimitSimpleSearch )
             , ( "domains", GBArg.variable <| GBVar.required "domains" .searchDomains (GBVar.list GBVar.string) )
             , ( "text", GBArg.variable <| GBVar.required "text" .searchString GBVar.string )
             ]
                ++ (paginationArguments prevResponse paginationTarget)
            )
        |> GB.extract
        |> GB.queryDocument


queryAuthorSearch :
    Maybe (Page Document)
    -> PaginationTarget
    -> GB.Document GB.Query (Page Document) { vars | searchString : String }
queryAuthorSearch prevResponse paginationTarget =
    valueSpecDocument
        |> connectionPage
        |> GB.field "authorSearch"
            ([ ( "text", GBArg.variable <| GBVar.required "text" .searchString GBVar.string ) ]
                ++ (paginationArguments prevResponse paginationTarget)
            )
        |> GB.extract
        |> GB.queryDocument


paginationArguments : Maybe (Page node) -> PaginationTarget -> List ( String, GBArg.Value vars )
paginationArguments prevResponse target =
    let
        paginationSizeArg =
            GBArg.int paginationSize

        anchor direction =
            case prevResponse of
                Nothing ->
                    []

                Just page ->
                    case
                        (page.pageInfo
                            |> if direction then
                                .endCursor
                               else
                                .startCursor
                        )
                    of
                        Nothing ->
                            []

                        Just cursor ->
                            [ ( if direction then
                                    "after"
                                else
                                    "before"
                              , GBArg.string cursor
                              )
                            ]
    in
        case target of
            First ->
                [ ( "first", paginationSizeArg ) ]

            Next ->
                [ ( "first", paginationSizeArg ) ] ++ anchor True

            Last ->
                [ ( "last", paginationSizeArg ) ]

            Previous ->
                [ ( "last", paginationSizeArg ) ] ++ anchor False


{-| Extract node objects from paginated Relay connections yielding a plain list (without additional page- and cursor-info)
-}
connectionList :
    GB.ValueSpec GB.NonNull GB.ObjectType result vars
    -> GB.ValueSpec GB.NonNull GB.ObjectType (List result) vars
connectionList spec =
    spec
        |> GB.field "node" []
        |> GB.extract
        |> GB.list
        |> GB.field "edges" []
        |> GB.extract


{-| Extract node objects from paginated Relay connections yielding a Page type that includes all additional paging info
-}
connectionPage :
    GB.ValueSpec GB.NonNull GB.ObjectType result vars
    -> GB.ValueSpec GB.NonNull GB.ObjectType (Page result) vars
connectionPage spec =
    GB.object Page
        |> GB.with (GB.field "totalCount" [] GB.int)
        |> GB.with
            (PageInfo
                |> GB.object
                |> GB.with (GB.field "hasNextPage" [] GB.bool)
                |> GB.with (GB.field "hasPreviousPage" [] GB.bool)
                |> GB.with (GB.field "startCursor" [] (GB.nullable GB.string))
                |> GB.with (GB.field "endCursor" [] (GB.nullable GB.string))
                |> GB.field "pageInfo" []
            )
        |> GB.with
            (Edge
                |> GB.object
                |> GB.with (GB.field "cursor" [] GB.string)
                |> GB.with (GB.field "node" [] spec)
                |> GB.list
                |> GB.field "edges" []
            )


sendQuery :
    GB.Document GB.Query model vars
    -> vars
    -> (model -> Msg)
    -> Cmd Msg
sendQuery queryDocument variables successTagger =
    queryDocument
        |> GB.request variables
        |> GraphQLClient.sendQuery "http://localhost:5000/graphql"
        |> Task.attempt (Result.Extra.unpack GraphQLClientError successTagger)


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
    | GraphQLClientError GraphQLClient.Error
    | DocumentsPageResult (Page Document)
    | PaginationTarget PageView PaginationTarget


type PaginationTarget
    = First
    | Last
    | Next
    | Previous


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
        , sendSearchQuery First model
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
                , sendSearchQuery First model1
                )

        SetSearchType searchType ->
            let
                model1 =
                    { model | searchType = searchType }
            in
                ( model1
                , sendSearchQuery First model1
                )

        PaginationTarget pageView target ->
            ( model
            , case pageView of
                ViewDocuments ->
                    sendSearchQuery target model
            )

        GraphQLClientError graphQLClientError ->
            let
                _ =
                    Debug.log "... GraphQLClient Error" graphQLClientError
            in
                ( model, Cmd.none )

        DocumentsPageResult documents ->
            ( { model | documents = Just documents }
            , Cmd.none
            )


sendSearchQuery : PaginationTarget -> Model -> Cmd Msg
sendSearchQuery paginationTarget model =
    case model.searchType of
        SimpleSearch simpleSearchDomain ->
            sendQuery
                (querySimpleSearch model.documents paginationTarget)
                { searchString = model.searchString
                , searchDomains =
                    case simpleSearchDomain of
                        SearchAttributes ->
                            [ "attrs" ]

                        SearchFulltext ->
                            [ "fulltext" ]

                        SearchAll ->
                            [ "attrs", "fulltext" ]
                }
                DocumentsPageResult

        AuthorSearch ->
            sendQuery
                (queryAuthorSearch model.documents paginationTarget)
                { searchString = 
                    model.searchString
                        |> String.words
                        |> List.map (flip String.append <| ":*")
                        |> String.join " & "
                }
                DocumentsPageResult


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h2 []
            [ Html.text "mediaTUM HSB Demo 2017-12-20"
            , Html.div [ Html.Attributes.class "color" ]
                [ Html.text "PostgreSQL · PostGraphQL · GraphQL · Elm" ]
            ]
        , Html.hr [] []
        , viewSearchControls model
        , Html.hr [] []
        , model.documents
            |> viewResponse (PaginationTarget ViewDocuments) (viewPage viewDocument)
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
    (PaginationTarget -> Msg)
    -> (Page node -> Html Msg)
    -> Maybe (Page node)
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


viewPage : (node -> Html Msg) -> Page node -> Html Msg
viewPage viewNode page =
    Html.div []
        [ Html.div []
            (List.map
                (.node >> viewNode)
                page.edges
            )
        ]


viewNumberOfResults : Maybe (Page mode) -> Html msg
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
                            -- TODO: Following code is only valid for a seimpleSearch (i.e. with a limit)
                            if page.totalCount == sizeLimitSimpleSearch then
                                ">= "
                            else
                                ""
                        , Html.text <| toString page.totalCount
                        ]
            )
        ]


viewPaginationButtons : Maybe (Page node) -> (PaginationTarget -> Msg) -> Html Msg
viewPaginationButtons response targetTagger =
    let
        hasNextPage =
            Maybe.Extra.unwrap False (.pageInfo >> .hasNextPage) response

        hasPreviousPage =
            Maybe.Extra.unwrap False (.pageInfo >> .hasPreviousPage) response

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
              viewButton hasPreviousPage "First" (targetTagger First)

            -- viewButton hasPreviousPage "Prev" (targetTagger Previous),
            , viewButton hasNextPage "Next" (targetTagger Next)

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
