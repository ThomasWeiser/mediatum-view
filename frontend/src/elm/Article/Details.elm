module Article.Details exposing
    ( Context
    , Model
    , Msg
    , Return(..)
    , initialModel
    , update
    , view
    )

import Api
import Api.Mutations
import Api.Queries
import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (Document, DocumentAttribute, DocumentId)
import Document
import Graphql.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Maybe.Extra
import Query
import RemoteData
import Utils


type alias Context =
    { cache : Cache.Model
    , detailsQuery : Query.DetailsQuery
    }


type Return
    = NoReturn
    | UpdateCacheWithModifiedDocument Document


type alias Model =
    { editAttributeKey : String
    , editAttributeValue : String
    , mutationState : MutationState
    }


type MutationState
    = Init
    | Pending
    | CannotUpdateKey String
    | MutationError Api.Error


type Msg
    = ApiMutationResponse DocumentId String (Api.Response (Maybe Document))
    | SetAttributeKey String
    | SetAttributeValue String
    | SubmitMutation DocumentId


initialModel : Context -> Model
initialModel context =
    { editAttributeKey = ""
    , editAttributeValue = ""
    , mutationState = Init
    }


update : Context -> Msg -> Model -> ( Model, Cmd Msg, Return )
update context msg model =
    case msg of
        ApiMutationResponse _ _ (Err err) ->
            ( { model | mutationState = MutationError err }
            , Cmd.none
            , NoReturn
            )

        ApiMutationResponse _ key (Ok Nothing) ->
            ( { model | mutationState = CannotUpdateKey key }
            , Cmd.none
            , NoReturn
            )

        ApiMutationResponse id _ (Ok (Just result)) ->
            ( { model
                | mutationState = Init
              }
                |> initEditAttributeValue context
            , Cmd.none
            , UpdateCacheWithModifiedDocument result
            )

        SetAttributeKey key ->
            ( { model | editAttributeKey = key }
                |> initEditAttributeValue context
            , Cmd.none
            , NoReturn
            )

        SetAttributeValue value ->
            ( { model | editAttributeValue = value }
            , Cmd.none
            , NoReturn
            )

        SubmitMutation documentId ->
            ( { model | mutationState = Pending }
            , Api.sendMutationRequest
                (ApiMutationResponse documentId model.editAttributeKey)
                (Api.Mutations.updateDocumentAttribute
                    documentId
                    model.editAttributeKey
                    model.editAttributeValue
                )
            , NoReturn
            )


initEditAttributeValue : Context -> Model -> Model
initEditAttributeValue context model =
    case
        Cache.get
            context.cache.documents
            context.detailsQuery.documentId
    of
        RemoteData.Success (Just document) ->
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


view : Context -> Model -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "details" ]
        [ case
            Cache.get
                context.cache.documents
                context.detailsQuery.documentId
          of
            RemoteData.NotAsked ->
                -- Should never happen
                Icons.spinner

            RemoteData.Loading ->
                Icons.spinner

            RemoteData.Failure error ->
                viewApiError error

            RemoteData.Success (Just document) ->
                viewDocument model document

            RemoteData.Success Nothing ->
                Html.span []
                    [ Html.text "Document with id "
                    , Html.text
                        (context.detailsQuery.documentId
                            |> Data.Types.documentIdToInt
                            |> String.fromInt
                        )
                    , Html.text " not available"
                    ]
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


viewAttribute : DocumentAttribute -> Html msg
viewAttribute attribute =
    case attribute.value of
        Just value ->
            Html.tr []
                [ Html.td [] [ Html.text attribute.name ]
                , Html.td [] [ Html.text value ]
                ]

        Nothing ->
            Html.text ""


viewApiError : Api.Error -> Html msg
viewApiError error =
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
                        viewApiError error
                ]
            ]
        ]


viewError : String -> Html msg
viewError defect =
    Html.div
        [ Html.Attributes.class "error" ]
        [ Html.text defect ]
