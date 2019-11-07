module UI.Tree exposing
    ( Model
    , Msg
    , Return(..)
    , expandPresentationFolder
    , initialModel
    , needs
    , update
    , view
    , viewBreadcrumbs
    )

import Api
import Api.Queries
import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (Folder, FolderCounts, FolderId)
import Folder
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Presentation exposing (Presentation(..))
import RemoteData
import Route
import Route.Url
import Sort.Dict
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
        |> getPathAsFarAsCached context.cache
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


getParentId : Cache.Model -> FolderId -> ApiData (Maybe FolderId)
getParentId cache id =
    Cache.get cache.folders id
        |> RemoteData.map .parent


getPath : Cache.Model -> FolderId -> ApiData (List FolderId)
getPath cache id =
    getParentId cache id
        |> RemoteData.andThen
            (Maybe.Extra.unwrap
                (RemoteData.Success [ id ])
                (getPath cache
                    >> RemoteData.map ((::) id)
                )
            )


getPathAsFarAsCached : Cache.Model -> Maybe FolderId -> List FolderId
getPathAsFarAsCached cache =
    Maybe.Extra.unwrap
        []
        (\id ->
            id
                :: (getParentId cache id
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> getPathAsFarAsCached cache
                   )
        )


isOnPath : Cache.Model -> FolderId -> Maybe FolderId -> Bool
isOnPath cache requestedId =
    Maybe.Extra.unwrap
        False
        (\pathId ->
            (requestedId == pathId)
                || isOnPath cache
                    requestedId
                    (getParentId cache pathId
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                    )
        )


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
                    [ viewFolder context model maybeFolderCounts id ]
            )
            folderIds


viewListOfFoldersLoading : Html Msg
viewListOfFoldersLoading =
    -- TODO: Currenty unused. Need a more general solution for showing RemoteData.Loading states.
    Html.ul [ Html.Attributes.class "folder-list" ]
        [ Html.li [] [ Html.text "..." ]
        ]


viewFolder : Context -> Model -> Maybe FolderCounts -> FolderId -> Html Msg
viewFolder context model maybeFolderCounts id =
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
                        && isOnPath context.cache id presentationFolderId
            in
            Html.div []
                [ Html.div
                    [ Html.Events.onClick (Select id) ]
                    [ Folder.view
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


viewBreadcrumbs : Context -> Model -> Maybe FolderId -> Html msg
viewBreadcrumbs context model maybeFolderId =
    Html.span [] <|
        case maybeFolderId of
            Nothing ->
                [ Html.text "(no specific path)" ]

            Just folderId ->
                getPath context.cache folderId
                    |> RemoteData.unwrap
                        [ Html.text "..." ]
                        (List.reverse
                            >> List.map
                                (\idPathSegment ->
                                    Html.span []
                                        [ Cache.get context.cache.folders idPathSegment
                                            |> RemoteData.unwrap
                                                (Html.text "...")
                                                (\folder ->
                                                    Html.a
                                                        [ folder.id
                                                            |> Data.Types.folderIdToInt
                                                            |> Data.Types.nodeIdFromInt
                                                            |> Route.fromOneId
                                                            |> Route.Url.toString
                                                            |> Html.Attributes.href
                                                        ]
                                                        [ Html.text folder.name ]
                                                )
                                        ]
                                )
                            >> List.intersperse
                                (Html.span [] [ Html.text " > " ])
                        )