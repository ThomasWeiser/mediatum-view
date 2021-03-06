module UI.Article.Details exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg
@docs initialModel
@docs update
@docs view

-}

import Api
import Api.Mutations
import Cache exposing (Cache)
import Entities.Document as Document exposing (Document)
import Entities.Markup
import Entities.Residence as Residence exposing (Residence)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Nonempty
import Maybe.Extra
import RemoteData
import Types exposing (DocumentIdFromSearch)
import Types.Config as Config exposing (Config)
import Types.Config.MasksConfig as MasksConfig
import Types.Id exposing (DocumentId)
import Types.Localization as Localization
import Types.Route exposing (Route)
import UI.Icons
import UI.Widgets.Breadcrumbs
import Utils.Html


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , route : Route
    , documentIdFromSearch : DocumentIdFromSearch
    }


{-| -}
type Return
    = NoReturn
    | UpdateCacheWithModifiedDocument Document


{-| -}
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


{-| -}
type Msg
    = ApiMutationResponse DocumentId String (Api.Response (Maybe Document))
    | SetAttributeKey String
    | SetAttributeValue String
    | SubmitMutation DocumentId


{-| -}
initialModel : Model
initialModel =
    { editAttributeKey = ""
    , editAttributeValue = ""
    , mutationState = Init
    }


{-| -}
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
                (Api.withOperationName "ModifyDocumentAttribute")
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
            ( Config.getMaskName MasksConfig.MaskForDetails context.config
            , context.documentIdFromSearch
            )
    of
        RemoteData.Success document ->
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
                            , Document.attributeValue firstKey document |> Maybe.withDefault Entities.Markup.empty
                            )
            in
            { model
                | editAttributeKey = key1
                , editAttributeValue =
                    Entities.Markup.plainText value1
            }

        _ ->
            model


{-| -}
view : Context -> Model -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "details" ]
        [ case
            RemoteData.map2 Tuple.pair
                (Cache.get context.cache.documents
                    ( Config.getMaskName MasksConfig.MaskForDetails context.config
                    , context.documentIdFromSearch
                    )
                )
                (Cache.get context.cache.residence context.documentIdFromSearch.id)
          of
            RemoteData.NotAsked ->
                -- Should never happen
                UI.Icons.spinner

            RemoteData.Loading ->
                UI.Icons.spinner

            RemoteData.Failure error ->
                Utils.Html.viewApiError error

            RemoteData.Success ( document, residence ) ->
                viewDocument context model document residence
        ]


viewDocument : Context -> Model -> Document -> Residence -> Html Msg
viewDocument context model document residence =
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
        , viewSearchMatching context.config document.searchMatching
        , viewResidence context residence
        , viewEditAttribute model document
        ]


viewSearchMatching : Config -> Maybe Document.SearchMatching -> Html msg
viewSearchMatching config =
    Maybe.Extra.unwrap
        { en = "", de = "" }
        (\{ attributes, fulltext } ->
            case ( attributes, fulltext ) of
                ( False, False ) ->
                    { en = "", de = "" }

                ( True, False ) ->
                    { en = "Search term found in metadata"
                    , de = "Suchbegriff in Metadaten gefunden"
                    }

                ( False, True ) ->
                    { en = "Search term found in fulltext"
                    , de = "Suchbegriff in Volltext gefunden"
                    }

                ( True, True ) ->
                    { en = "Search term found in metadata and fulltext"
                    , de = "Suchbegriff in Metadaten und Volltext gefunden"
                    }
        )
        >> Localization.text config


viewAttribute : Document.Attribute -> Html msg
viewAttribute attribute =
    case attribute.value of
        Just value ->
            Html.tr
                [ Html.Attributes.class "attribute" ]
                [ Html.td [] [ Html.text attribute.name ]
                , Html.td []
                    [ Entities.Markup.view value
                    ]
                ]

        Nothing ->
            Html.text ""


viewResidence : Context -> Residence -> Html msg
viewResidence context residence =
    Html.div
        [ Html.Attributes.class "residence" ]
        [ Html.div
            [ Html.Attributes.class "title" ]
            [ Localization.text context.config
                { en = "Occurrences:"
                , de = "Vorkommen:"
                }
            ]
        , Html.ul [] <|
            List.map
                (\lineage ->
                    Html.li []
                        [ lineage
                            |> List.Nonempty.toList
                            |> Just
                            |> UI.Widgets.Breadcrumbs.view context
                        ]
                )
                (Residence.limitToToplevelFolders context.config residence)
        ]


viewEditAttribute : Model -> Document -> Html Msg
viewEditAttribute model document =
    -- TODO: Editing feature will be removed soon. View function not localized for now.
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
                , Html.Events.onInput SetAttributeKey
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
                , Html.Events.onInput SetAttributeValue
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
