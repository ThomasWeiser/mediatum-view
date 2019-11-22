module UI.Tree exposing
    ( Model
    , Msg
    , Return(..)
    , expandPresentationFolder
    , initialModel
    , needs
    , update
    , view
    )

import Cache exposing (ApiData)
import Cache.Derive
import Entities.Folder as Folder exposing (Folder)
import Entities.FolderCounts as FolderCounts exposing (FolderCounts)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import Presentation exposing (Presentation(..))
import RemoteData
import Sort.Dict
import Types exposing (FolderDisplay(..))
import Types.Id as Id exposing (FolderId)
import UI.Icons
import Utils


type alias Context =
    { cache : Cache.Model
    , presentation : Presentation
    }


type Return
    = NoReturn
    | UserSelection FolderId


type alias Model =
    { collapsedPresentationFolder : Maybe FolderId
    }


type Msg
    = Select FolderId


initialModel : Model
initialModel =
    { collapsedPresentationFolder = Nothing
    }


needs : Context -> Model -> Cache.Needs
needs context model =
    getPresentationFolderId context
        |> Cache.Derive.getPathAsFarAsCached context.cache
        |> Cache.NeedSubfolders


expandPresentationFolder : Model -> Model
expandPresentationFolder model =
    { model
        | collapsedPresentationFolder = Nothing
    }


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


view : Context -> Model -> Maybe FolderCounts -> Html Msg
view context model maybeFolderCounts =
    Html.div []
        [ case context.cache.rootFolderIds of
            RemoteData.Success rootIds ->
                viewListOfFolders context model maybeFolderCounts rootIds

            -- TODO: RemoteData.Failure error ->
            noSuccess ->
                Html.text (Debug.toString noSuccess)
        ]


viewListOfFolders : Context -> Model -> Maybe FolderCounts -> List FolderId -> Html Msg
viewListOfFolders context model maybeFolderCounts folderIds =
    Html.ul [ Html.Attributes.class "folder-list" ] <|
        List.map
            (\id ->
                Html.li []
                    [ viewFolderTree context model maybeFolderCounts id ]
            )
            folderIds


viewListOfFoldersLoading : Html Msg
viewListOfFoldersLoading =
    -- TODO: Currenty unused. Need a more general solution for showing RemoteData.Loading states.
    Html.ul [ Html.Attributes.class "folder-list" ]
        [ Html.li [] [ Html.text "..." ]
        ]


viewFolderTree : Context -> Model -> Maybe FolderCounts -> FolderId -> Html Msg
viewFolderTree context model maybeFolderCounts id =
    case Cache.get context.cache.folders id of
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
            Html.div []
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
                    case Cache.get context.cache.subfolderIds id of
                        RemoteData.Success subfolderIds ->
                            viewListOfFolders context model maybeFolderCounts subfolderIds

                        noSuccess ->
                            Html.text (Debug.toString noSuccess)

                  else
                    Html.text ""
                ]

        noSuccess ->
            Html.text (Debug.toString noSuccess)


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
