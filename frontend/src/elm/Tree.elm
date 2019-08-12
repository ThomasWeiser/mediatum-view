module Tree exposing
    ( Model
    , Msg
    , Return(..)
    , initialModel
    , needs
    , selectFolder
    , update
    , view
    , viewBreadcrumbs
    )

import Api
import Api.Queries
import Data.Cache as Cache exposing (ApiData)
import Data.Types exposing (Folder, FolderCounts, FolderId)
import Dict exposing (Dict)
import Dict.Extra
import Folder
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import RemoteData
import Route
import Utils


type alias Context =
    { cache : Cache.Model
    }


type Return
    = NoReturn
    | UserSelection Folder


type alias Model =
    { selection : List FolderId
    , showSubselection : Bool
    }


type Msg
    = Select FolderId


initialModel : Model
initialModel =
    { selection = []
    , showSubselection = True
    }


selectedFolder : Context -> Model -> Maybe Folder
selectedFolder context model =
    -- TODO: Poss. return Maybe (FolderId)
    List.head model.selection
        |> Maybe.andThen
            (\headId ->
                Dict.get headId context.cache.folders
                    |> Maybe.withDefault RemoteData.NotAsked
                    |> RemoteData.toMaybe
            )


needs : Model -> Cache.Needs
needs model =
    -- TODO: Poss. need subfolders of selection head only if showSubselection is true.
    [ Cache.NeedSubfolders model.selection ]


update : Context -> Msg -> Model -> ( Model, Return )
update context msg model =
    case msg of
        Select id ->
            model
                |> selectFolder context id
                |> (\model1 ->
                        ( model1
                        , if List.head model.selection /= Just id then
                            Maybe.Extra.unwrap
                                NoReturn
                                UserSelection
                                (selectedFolder context model1)

                          else
                            NoReturn
                        )
                   )


getParentId : Cache.Model -> FolderId -> ApiData (Maybe FolderId)
getParentId cache id =
    Dict.get id cache.folders
        |> Maybe.withDefault RemoteData.NotAsked
        |> RemoteData.map .parent


getPath : Cache.Model -> FolderId -> ApiData (List FolderId)
getPath cache id =
    getParentId cache id
        |> RemoteData.andThen
            (Maybe.Extra.unwrap
                (RemoteData.Success [ id ])
                -- TODO poss. simplify dot free and/or RemoteData.map
                (\parentId ->
                    getPath cache parentId
                        |> RemoteData.andThen
                            (\parentIds ->
                                RemoteData.Success (id :: parentIds)
                            )
                )
            )


selectFolder : Context -> FolderId -> Model -> Model
selectFolder context id model =
    let
        alreadySelected =
            List.head model.selection == Just id
    in
    { model
        | selection =
            getPath context.cache id
                -- TODO: This look odd.
                -- If the path data isn't already cache (possible?) then the selection
                -- won't get corrected when the cache is filled.
                -- We probably should only keep the selection head in the model.
                |> RemoteData.withDefault []
        , showSubselection = not alreadySelected || not model.showSubselection
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
    Html.ul [ Html.Attributes.class "folder-list" ]
        [ Html.li [] [ Html.text "..." ]
        ]


viewFolder : Context -> Model -> FolderCounts -> FolderId -> Html Msg
viewFolder context model folderCounts id =
    let
        isSelectedFolder =
            List.head model.selection == Just id

        expanded =
            List.member id model.selection
                && (not isSelectedFolder || model.showSubselection)
    in
    case Dict.get id context.cache.folders |> Maybe.withDefault RemoteData.NotAsked of
        {-
           Nothing ->
               -- Cache miss. Should never happen,
               -- because only cached folders are getting linked.
               Html.div [] [ Html.text "..." ]
        -}
        RemoteData.Success folder ->
            Html.div []
                [ Html.div
                    [ Html.Events.onClick (Select id) ]
                    [ Folder.view
                        folder
                        (Dict.get folder.id folderCounts)
                        isSelectedFolder
                        expanded
                    ]
                , if expanded then
                    case Dict.get id context.cache.subfolderIds |> Maybe.withDefault RemoteData.NotAsked of
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
    Html.span []
        (getPath context.cache id
            |> RemoteData.unwrap
                [ Html.text "..." ]
                (\path ->
                    path
                        |> List.reverse
                        |> List.map
                            (\id1 ->
                                Html.span []
                                    [ Dict.get id1 context.cache.folders
                                        |> Maybe.withDefault RemoteData.NotAsked
                                        |> RemoteData.unwrap
                                            (Html.text "...")
                                            (\folder ->
                                                Html.a
                                                    [ folder.id
                                                        |> Folder.idToInt
                                                        |> Route.NodeId
                                                        |> Route.toString
                                                        |> Html.Attributes.href
                                                    ]
                                                    [ Html.text folder.name ]
                                            )
                                    ]
                            )
                        |> List.intersperse
                            (Html.span [] [ Html.text " > " ])
                )
        )
