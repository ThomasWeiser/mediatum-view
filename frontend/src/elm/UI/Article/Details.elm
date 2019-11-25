module UI.Article.Details exposing
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
import Cache
import Entities.Document as Document exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import RemoteData
import Types.Id as Id exposing (DocumentId)
import UI.Icons
import Utils
import Utils.Html


type alias Context =
    { cache : Cache.Model
    , documentId : DocumentId
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


initialModel : Model
initialModel =
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
            context.documentId
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
                context.documentId
          of
            RemoteData.NotAsked ->
                -- Should never happen
                UI.Icons.spinner

            RemoteData.Loading ->
                UI.Icons.spinner

            RemoteData.Failure error ->
                Utils.Html.viewApiError error

            RemoteData.Success (Just document) ->
                viewDocument model document

            RemoteData.Success Nothing ->
                Html.span []
                    [ Html.text "Document with id "
                    , Html.text (context.documentId |> Id.toString)
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


viewAttribute : Document.Attribute -> Html msg
viewAttribute attribute =
    case attribute.value of
        Just value ->
            Html.tr []
                [ Html.td [] [ Html.text attribute.name ]
                , Html.td [] [ Html.text value ]
                ]

        Nothing ->
            Html.text ""


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
                        UI.Icons.spinner

                    CannotUpdateKey key ->
                        Utils.Html.viewError <| "Cannot update key \"" ++ key ++ "\". It's not present in the JSON attributes of the document's node"

                    MutationError error ->
                        Utils.Html.viewApiError error
                ]
            ]
        ]
