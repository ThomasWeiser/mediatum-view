module UI.Tree exposing
    ( Context
    , Return(..)
    , Model
    , Msg
    , initialModel
    , needs
    , expandPresentationFolder
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
@docs expandPresentationFolder
@docs update
@docs view

-}

import Cache exposing (ApiData, Cache)
import Cache.Derive
import Entities.Folder as Folder exposing (Folder)
import Entities.FolderCounts exposing (FolderCounts)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import RemoteData exposing (RemoteData)
import Sort.Dict
import Types exposing (FolderDisplay(..))
import Types.Id exposing (FolderId)
import Types.Needs
import Types.Presentation as Presentation exposing (Presentation(..))
import UI.Icons
import Utils
import Utils.Html


{-| -}
type alias Context =
    { cache : Cache
    , presentation : Presentation
    }


{-| -}
type Return
    = NoReturn
    | UserSelection FolderId


{-| -}
type alias Model =
    { collapsedPresentationFolder : Maybe FolderId
    }


{-| -}
type Msg
    = Select FolderId


{-| -}
initialModel : Model
initialModel =
    { collapsedPresentationFolder = Nothing
    }


{-| -}
needs : Context -> Model -> Cache.Needs
needs context model =
    getPresentationFolderId context
        |> Cache.Derive.getPathAsFarAsCached context.cache
        |> Cache.NeedSubfolders
        |> Types.Needs.atomic


{-| -}
expandPresentationFolder : Model -> Model
expandPresentationFolder model =
    { model
        | collapsedPresentationFolder = Nothing
    }


{-| -}
update : Context -> Msg -> Model -> ( Model, Return )
update context msg model =
    case msg of
        Select id ->
            if getPresentationFolderId context == Just id then
                ( { model
                    | collapsedPresentationFolder =
                        (model.collapsedPresentationFolder == Just id)
                            |> Utils.ifElse
                                Nothing
                                (Just id)
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
            context.cache.rootFolderIds
        ]


viewListOfFolders : Context -> Model -> Maybe FolderCounts -> ApiData (List FolderId) -> Html Msg
viewListOfFolders context model maybeFolderCounts apiDataFolderIds =
    Html.ul [ Html.Attributes.class "folder-list" ] <|
        case apiDataFolderIds of
            RemoteData.NotAsked ->
                [ Html.li [] [ Html.text "..." ] ]

            RemoteData.Loading ->
                [ Html.li [] [ Html.text "..." ] ]

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
                [ Html.text "..." ]

            RemoteData.Loading ->
                [ Html.text "..." ]

            RemoteData.Success folder ->
                let
                    presentationFolderId =
                        getPresentationFolderId context

                    isSelectedFolder =
                        presentationFolderId == Just id

                    expanded =
                        Folder.isRoot folder
                            || (model.collapsedPresentationFolder /= Just id)
                            && Cache.Derive.isOnPath context.cache id presentationFolderId
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
        ([ if Folder.hasSubfolder folder then
            UI.Icons.expando

           else
            UI.Icons.leaf
         , Html.span
            [ Html.Attributes.class "folder-name" ]
            [ Html.text folder.name ]
         ]
            ++ (case maybeCount of
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
