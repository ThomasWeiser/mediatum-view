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
import Html.Events
import Icons
import Maybe.Extra
import Query
import Utils


type alias Context =
    { detailsQuery : Query.DetailsQuery
    }


type alias Model =
    { remoteDocument : RemoteDocument
    , editAttributeKey : String
    , editAttributeValue : String
    , mutationState : MutationState
    }


type RemoteDocument
    = Loading
    | Success Document
    | NotFound DocumentId
    | QueryError Graphql.Extra.StrippedError


type MutationState
    = Init
    | Pending
    | CannotUpdateKey String
    | MutationError Graphql.Extra.StrippedError


type Msg
    = ApiQueryResponse DocumentId (Api.Response (Maybe Document))
    | ApiMutationResponse DocumentId String (Api.Response (Maybe Document))
    | SetAttributeKey String
    | SetAttributeValue String
    | SubmitMutation DocumentId


init : Context -> ( Model, Cmd Msg )
init context =
    ( { remoteDocument = Loading
      , editAttributeKey = ""
      , editAttributeValue = ""
      , mutationState = Init
      }
    , Api.makeQueryRequest
        (ApiQueryResponse context.detailsQuery.documentId)
        (Api.queryDocumentDetails context.detailsQuery.documentId)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiQueryResponse _ (Err err) ->
            ( { model | remoteDocument = QueryError err }
            , Cmd.none
            )

        ApiQueryResponse id (Ok result) ->
            ( { model
                | remoteDocument =
                    Maybe.Extra.unwrap (NotFound id) Success result
              }
            , Cmd.none
            )

        ApiMutationResponse _ _ (Err err) ->
            ( { model | mutationState = MutationError err }
            , Cmd.none
            )

        ApiMutationResponse _ key (Ok Nothing) ->
            ( { model | mutationState = CannotUpdateKey key }
            , Cmd.none
            )

        ApiMutationResponse id _ (Ok (Just result)) ->
            ( { model
                | remoteDocument = Success result
                , mutationState = Init
                , editAttributeValue = ""
              }
            , Cmd.none
            )

        SetAttributeKey key ->
            ( { model | editAttributeKey = key }
            , Cmd.none
            )

        SetAttributeValue value ->
            ( { model | editAttributeValue = value }
            , Cmd.none
            )

        SubmitMutation documentId ->
            ( { model | mutationState = Pending }
            , Api.makeMutationRequest
                (ApiMutationResponse documentId model.editAttributeKey)
                (Api.updateDocumentAttribute
                    documentId
                    model.editAttributeKey
                    model.editAttributeValue
                )
            )


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "details" ]
        [ case model.remoteDocument of
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

            QueryError error ->
                viewGraphqlError error
        , viewEditAttribute model
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


viewGraphqlError : Graphql.Extra.StrippedError -> Html msg
viewGraphqlError error =
    viewError (Graphql.Extra.errorToString error)


viewEditAttribute : Model -> Html Msg
viewEditAttribute model =
    let
        maybeDocumentId =
            case model.remoteDocument of
                Success document ->
                    Just document.id

                _ ->
                    Nothing

        formDisabled =
            model.mutationState == Pending
    in
    Html.form
        (Maybe.Extra.unwrap []
            (\id -> [ Html.Events.onSubmit (SubmitMutation id) ])
            maybeDocumentId
        )
        [ Html.div [ Html.Attributes.class "edit-attribute" ]
            [ Html.hr [] []
            , Html.div [] [ Html.text "Edit an Attribute of this Document:" ]
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Key"
                , Html.Attributes.value model.editAttributeKey
                , Html.Attributes.disabled formDisabled
                , Utils.onChange SetAttributeKey
                ]
                []
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Value"
                , Html.Attributes.value model.editAttributeValue
                , Html.Attributes.disabled formDisabled
                , Utils.onChange SetAttributeValue
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.disabled (formDisabled || model.editAttributeKey == "")
                ]
                [ Html.text "Ok" ]
            , Html.div []
                [ case model.mutationState of
                    Init ->
                        Html.text ""

                    Pending ->
                        Icons.spinner

                    CannotUpdateKey key ->
                        viewError <| "Cannot update key \"" ++ key ++ "\""

                    MutationError error ->
                        viewGraphqlError error
                ]
            ]
        ]


viewError : String -> Html msg
viewError defect =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text defect ]
