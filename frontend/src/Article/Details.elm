module Article.Details exposing
    ( Context
    , Model
    , Msg
    , init
    , update
    , view
    )

import Api
import Document exposing (Attribute, Document, DocumentId)
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Icons
import Maybe.Extra
import Query


type alias Context =
    { detailsQuery : Query.DetailsQuery
    }


type Model
    = Loading
    | Success Document
    | NotFound DocumentId
    | Error Graphql.Extra.StrippedError


type Msg
    = ApiResponse DocumentId (Api.Response (Maybe Document))


init : Context -> ( Model, Cmd Msg )
init context =
    ( Loading
    , sendDocumentQuery context.detailsQuery.documentId
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponse _ (Err err) ->
            ( Error err
            , Cmd.none
            )

        ApiResponse id (Ok result) ->
            ( Maybe.Extra.unwrap
                (NotFound id)
                Success
                result
            , Cmd.none
            )


sendDocumentQuery : DocumentId -> Cmd Msg
sendDocumentQuery id =
    Api.makeRequest
        (ApiResponse id)
        (Api.queryDocumentDetails id)


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "details" ]
        [ case model of
            Loading ->
                Icons.spinner

            Success document ->
                viewDocument document

            NotFound id ->
                Html.span []
                    [ Html.text "Document with id "
                    , Html.text (Document.idToString id)
                    , Html.text " not available"
                    ]

            Error error ->
                viewError error
        ]


viewDocument : Document -> Html msg
viewDocument document =
    Html.div []
        [ Html.div [ Html.Attributes.class "header" ]
            [ Html.div [ Html.Attributes.class "metadatatype" ]
                [ Html.text document.metadatatypeName ]
            , Html.div [ Html.Attributes.class "author" ]
                [ Html.text document.name ]
            ]
        , Html.table []
            [ Html.tbody [] <|
                List.map
                    viewAttribute
                    document.attributes
            ]
        ]


viewAttribute : Attribute -> Html msg
viewAttribute attribute =
    case attribute.value of
        Just value ->
            Html.tr []
                [ Html.td [] [ Html.text attribute.name ]
                , Html.td [] [ Html.text value ]
                ]

        Nothing ->
            Html.text ""


viewError : Graphql.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (Graphql.Extra.errorToString error) ]
