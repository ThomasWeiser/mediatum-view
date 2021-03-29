module UI.Tree exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , needs
    , updateOnPresentationFolderId
    , update
    , view
    )

{-|

@docs Context
@docs Return
@docs Model
@docs Msg

@docs initialModel
@docs needs
@docs updateOnPresentationFolderId
@docs update
@docs view

-}

import Cache exposing (Cache)
import Cache.Derive
import Entities.Folder as Folder exposing (Folder)
import Entities.FolderCounts exposing (FolderCounts)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import RemoteData
import Sort.Dict
import Types exposing (FolderDisplay(..))
import Types.ApiData exposing (ApiData)
import Types.Config exposing (Config)
import Types.Id exposing (FolderId)
import Types.Needs
import Types.Presentation as Presentation exposing (Presentation(..))
import UI.Icons
import Utils.Html


{-| -}
type alias Context =
    { config : Config
    , cache : Cache
    , presentation : Presentation
    }


{-| -}
type Return
    = NoReturn
    | UserSelection FolderId


{-| The model represents the UI state of the tree.

Some notes on state management:

Selection of the tree nodes to be shown is basically informed by the context,
notably by the current presentation folder.
However, the expansion state of the current presentation folder if managed as a local UI state.

By default, the presentation folder is shown in expanded state.
If the user clicks on the presentation folder we show it in collapsed state.
If later the presentation folder changes or the user clicks the collapsed folder a second time,
then we show it in expanded state again.

For tracking the necessary state we use these fields as the local UI state:

  - `latestPresentationFolderId`, which is set in `updateOnPresentationFolderId`
  - `userCollapsedPresentationFolder`, which may be modified in `update` and in `updateOnPresentationFolderId`

-}
type alias Model =
    { latestPresentationFolderId : Maybe FolderId
    , userCollapsedPresentationFolder : Bool
    }


{-| -}
type Msg
    = Select FolderId


{-| -}
initialModel : Model
initialModel =
    { latestPresentationFolderId = Nothing
    , userCollapsedPresentationFolder = False
    }


{-| -}
needs : Context -> Model -> Cache.Needs
needs context model =
    getPresentationFolderId context
        |> Maybe.Extra.orElse (List.head context.config.toplevelFolderIds)
        |> Cache.Derive.getPathAsFarAsCached context.config context.cache
        |> Cache.NeedSubfolders
        |> Types.Needs.atomic


{-| -}
updateOnPresentationFolderId : Context -> Model -> Model
updateOnPresentationFolderId context model =
    let
        maybePresentationFolderId =
            getPresentationFolderId context
    in
    { model
        | latestPresentationFolderId = maybePresentationFolderId
        , userCollapsedPresentationFolder =
            if model.latestPresentationFolderId == maybePresentationFolderId then
                model.userCollapsedPresentationFolder

            else
                False
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Return )
update context msg model =
    case msg of
        Select id ->
            if getPresentationFolderId context == Just id then
                ( { model
                    | userCollapsedPresentationFolder = not model.userCollapsedPresentationFolder
                  }
                , NoReturn
                )

            else
                ( model
                , UserSelection id
                )


getPresentationFolderId : Context -> Maybe FolderId
getPresentationFolderId context =
    Presentation.getFolderId context.cache context.presentation


{-| -}
view : Context -> Model -> Maybe FolderCounts -> Html Msg
view context model maybeFolderCounts =
    Html.nav []
        [ viewListOfFolders
            context
            model
            maybeFolderCounts
            (RemoteData.Success context.config.toplevelFolderIds)
        ]


viewListOfFolders : Context -> Model -> Maybe FolderCounts -> ApiData (List FolderId) -> Html Msg
viewListOfFolders context model maybeFolderCounts apiDataFolderIds =
    Html.ul [ Html.Attributes.class "folder-list" ] <|
        case apiDataFolderIds of
            RemoteData.NotAsked ->
                [ Html.li [] [ UI.Icons.spinnerSmall ] ]

            RemoteData.Loading ->
                [ Html.li [] [ UI.Icons.spinnerSmall ] ]

            RemoteData.Success folderIds ->
                List.map
                    (\id ->
                        Html.li []
                            [ viewFolderTree context model maybeFolderCounts id ]
                    )
                    folderIds

            RemoteData.Failure apiError ->
                [ Html.li [] [ Utils.Html.viewApiError apiError ] ]


viewFolderTree : Context -> Model -> Maybe FolderCounts -> FolderId -> Html Msg
viewFolderTree context model maybeFolderCounts id =
    Html.div [] <|
        case Cache.get context.cache.folders id of
            RemoteData.NotAsked ->
                [ UI.Icons.spinnerSmall ]

            RemoteData.Loading ->
                [ UI.Icons.spinnerSmall ]

            RemoteData.Success folder ->
                let
                    presentationFolderId =
                        getPresentationFolderId context

                    expandSoleToplevelFolderOnPresentationWithoutFolder =
                        (context.config.toplevelFolderIds == [ id ])
                            && (presentationFolderId == Nothing)

                    collapsedByUser =
                        (model.latestPresentationFolderId == Just id)
                            && model.userCollapsedPresentationFolder

                    expanded =
                        expandSoleToplevelFolderOnPresentationWithoutFolder
                            || (not collapsedByUser
                                    && Cache.Derive.isOnPath context.config context.cache id presentationFolderId
                               )

                    isSelectedFolder =
                        presentationFolderId == Just id
                in
                [ Html.div
                    [ Html.Events.onClick (Select id) ]
                    [ viewFolderLine
                        folder
                        (maybeFolderCounts
                            |> Maybe.andThen (Sort.Dict.get folder.id)
                        )
                        isSelectedFolder
                        expanded
                    ]
                , if expanded then
                    viewListOfFolders
                        context
                        model
                        maybeFolderCounts
                        (Cache.get context.cache.subfolderIds id)

                  else
                    Html.text ""
                ]

            RemoteData.Failure apiError ->
                [ Utils.Html.viewApiError apiError ]


viewFolderLine : Folder -> Maybe Int -> Bool -> Bool -> Html msg
viewFolderLine folder maybeCount selected expanded =
    Html.div
        [ Html.Attributes.classList
            [ ( "folder-head", True )
            , ( "collection", folder.display == DisplayAsCollection )
            , ( "directory", folder.display == DisplayAsDirectory )
            , ( "collapsed", Folder.hasSubfolder folder && not expanded )
            , ( "expanded", Folder.hasSubfolder folder && expanded )
            , ( "leaf", not (Folder.hasSubfolder folder) )
            , ( "selected", selected )
            ]
        ]
        [ Html.div []
            [ if Folder.hasSubfolder folder then
                UI.Icons.expando

              else
                UI.Icons.leaf
            ]
        , Html.div
            [ Html.Attributes.class "folder-name" ]
            (Html.text folder.name
                :: (case maybeCount of
                        Nothing ->
                            []

                        Just count ->
                            [ Html.text " "
                            , Html.span
                                [ Html.Attributes.class "folder-count" ]
                                [ Html.text <| "(" ++ String.fromInt count ++ ")" ]
                            ]
                   )
            )
        ]
