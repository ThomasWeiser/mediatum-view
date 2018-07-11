module Article.Details
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        )

import Maybe.Extra
import Html exposing (Html)
import Html.Attributes
import Document exposing (Document, DocumentId, Attribute)
import Api
import Graphqelm.Extra
import Icons


type Model
    = Loading
    | Success Document
    | NotFound DocumentId
    | Error Graphqelm.Extra.StrippedError


type Msg
    = ApiResponse DocumentId (Api.Response (Maybe Document))


init : DocumentId -> ( Model, Cmd Msg )
init id =
    ( Loading
    , sendDocumentQuery id
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
                    , Html.text (toString id)
                    , Html.text " not available"
                    ]

            Error error ->
                viewError error
        ]


viewDocument : Document -> Html msg
viewDocument document =
    Html.table []
        [ Html.tbody [] <|
            List.map
                viewAttribute
                document.attributes
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


viewError : Graphqelm.Extra.StrippedError -> Html msg
viewError error =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text (toString error) ]
