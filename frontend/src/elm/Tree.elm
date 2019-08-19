module Tree exposing
    ( Model
    , Msg
    , Return(..)
    , initialModel
    , needs
    , showFolder
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
import RemoteData
import Route
import Route.Url
import Sort.Dict
import Utils


type alias Context =
    { cache : Cache.Model
    }


type Return
    = NoReturn
    | UserSelection Folder


type alias Model =
    { selection : Maybe FolderId
    , showSubselection : Bool
    }


type Msg
    = Select FolderId


initialModel : Model
initialModel =
    { selection = Nothing
    , showSubselection = True
    }


needs : Context -> Model -> Cache.Needs
needs context model =
    Cache.NeedSubfolders
        (getPathAsFarAsCached context.cache model.selection)


update : Context -> Msg -> Model -> ( Model, Return )
update context msg model =
    case msg of
        Select id ->
            ( { model
                | selection = Just id
                , showSubselection =
                    not (model.selection == Just id) || not model.showSubselection
              }
            , if model.selection /= Just id then
                Cache.get context.cache.folders id
                    |> RemoteData.toMaybe
                    |> Maybe.Extra.unwrap
                        NoReturn
                        UserSelection

              else
                NoReturn
            )


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


showFolder : FolderId -> Model -> Model
showFolder id model =
    { model
        | selection = Just id
        , showSubselection =
            -- TODO: Maybe always show subSelection.
            -- Currently we keep the state if the folder is already selected.
            not (model.selection == Just id) || model.showSubselection
    }


view : Context -> Model -> FolderCounts -> Html Msg
view context model folderCounts =
    Html.div []
        [ case context.cache.rootFolderIds of
            RemoteData.Success rootIds ->
                viewListOfFolders context model folderCounts rootIds

            -- TODO: RemoteData.Failure error ->
            noSuccess ->
                Html.text (Debug.toString noSuccess)
        ]


viewListOfFolders : Context -> Model -> FolderCounts -> List FolderId -> Html Msg
viewListOfFolders context model folderCounts folderIds =
    Html.ul [ Html.Attributes.class "folder-list" ] <|
        List.map
            (\id ->
                Html.li []
                    [ viewFolder context model folderCounts id ]
            )
            folderIds


viewListOfFoldersLoading : Html Msg
viewListOfFoldersLoading =
    -- TODO: Currenty unused. Need a more general solution for showing RemoteData.Loading states.
    Html.ul [ Html.Attributes.class "folder-list" ]
        [ Html.li [] [ Html.text "..." ]
        ]


viewFolder : Context -> Model -> FolderCounts -> FolderId -> Html Msg
viewFolder context model folderCounts id =
    let
        isSelectedFolder =
            model.selection == Just id

        expanded =
            (not isSelectedFolder || model.showSubselection)
                && isOnPath context.cache id model.selection
    in
    case Cache.get context.cache.folders id of
        RemoteData.Success folder ->
            Html.div []
                [ Html.div
                    [ Html.Events.onClick (Select id) ]
                    [ Folder.view
                        folder
                        (Sort.Dict.get folder.id folderCounts)
                        isSelectedFolder
                        expanded
                    ]
                , if expanded then
                    case Cache.get context.cache.subfolderIds id of
                        RemoteData.Success subfolderIds ->
                            viewListOfFolders context model folderCounts subfolderIds

                        noSuccess ->
                            Html.text (Debug.toString noSuccess)

                  else
                    Html.text ""
                ]

        noSuccess ->
            Html.text (Debug.toString noSuccess)


viewBreadcrumbs : Context -> Model -> FolderId -> Html msg
viewBreadcrumbs context model id =
    getPath context.cache id
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
        |> Html.span []
