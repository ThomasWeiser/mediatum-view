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
                |> initEditAttributeValue
            , Cmd.none
            )

        -- Edit Metadata
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
              }
                |> initEditAttributeValue
            , Cmd.none
            )

        SetAttributeKey key ->
            ( { model | editAttributeKey = key }
                |> initEditAttributeValue
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


initEditAttributeValue : Model -> Model
initEditAttributeValue model =
    case model.remoteDocument of
        Success document ->
            let
                ( key1, value1 ) =
                    case Document.attributeValue model.editAttributeKey document of
                        Just attributeValue ->
                            ( model.editAttributeKey, attributeValue )

                        Nothing ->
                            let
                                firstKey =
                                    List.head document.attributes
                                        |> Maybe.Extra.unwrap "" .field
                            in
                            ( firstKey
                            , Document.attributeValue firstKey document |> Maybe.withDefault ""
                            )
            in
            { model
                | editAttributeKey = key1
                , editAttributeValue = value1
            }

        _ ->
            model


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "details" ]
        [ case model.remoteDocument of
            Loading ->
                Icons.spinner

            Success document ->
                viewDocument model document

            NotFound id ->
                Html.span []
                    [ Html.text "Document with id "
                    , Html.text (Document.idToString id)
                    , Html.text " not available"
                    ]

            QueryError error ->
                viewGraphqlError error
        ]


viewDocument : Model -> Document -> Html Msg
viewDocument model document =
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
        , viewEditAttribute model document
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


viewEditAttribute : Model -> Document -> Html Msg
viewEditAttribute model document =
    let
        formDisabled =
            model.mutationState == Pending
    in
    Html.form
        [ Html.Events.onSubmit (SubmitMutation document.id) ]
        [ Html.div [ Html.Attributes.class "edit-attribute" ]
            [ Html.hr [] []
            , Html.div [] [ Html.text "Edit an Attribute of this Document:" ]
            , Html.select
                [ Html.Attributes.value model.editAttributeKey
                , Html.Attributes.disabled formDisabled
                , Utils.onChange SetAttributeKey
                ]
                (List.map
                    (\{ field, name } ->
                        Html.option
                            [ Html.Attributes.value field
                            , Html.Attributes.selected (model.editAttributeKey == field)
                            ]
                            [ Html.text name ]
                    )
                    document.attributes
                )
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
                        Html.text <| "Attribute key: " ++ model.editAttributeKey

                    Pending ->
                        Icons.spinner

                    CannotUpdateKey key ->
                        viewError <| "Cannot update key \"" ++ key ++ "\". It's not present in the JSON attributes of the document's node"

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
