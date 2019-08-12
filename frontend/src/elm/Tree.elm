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


selectedFolder : Context -> Model -> Maybe Folder
selectedFolder context model =
    -- TODO: Poss. return Maybe (FolderId)
    model.selection
        |> Maybe.andThen
            (\selectedFolderId ->
                Dict.get selectedFolderId context.cache.folders
                    |> Maybe.withDefault RemoteData.NotAsked
                    |> RemoteData.toMaybe
            )


needs : Context -> Model -> Cache.Needs
needs context model =
    [ Cache.NeedSubfolders
        (getPathAsFarAsCached context.cache model.selection)
    ]


update : Context -> Msg -> Model -> ( Model, Return )
update context msg model =
    case msg of
        Select id ->
            model
                |> selectFolder id
                |> (\model1 ->
                        ( model1
                        , if model.selection /= Just id then
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
            requestedId
                == pathId
                || isOnPath cache
                    requestedId
                    (getParentId cache pathId
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                    )
        )



-- TODO: Differentiate between
--   - selecting a folder after clicking on it
--   - selecting a folder from outside (e.g. when routibg to a generic node id)


selectFolder : FolderId -> Model -> Model
selectFolder id model =
    let
        alreadySelected =
            model.selection == Just id
    in
    { model
        | selection = Just id
        , showSubselection =
            not alreadySelected || not model.showSubselection
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
            model.selection == Just id

        expanded =
            (not isSelectedFolder || model.showSubselection)
                && isOnPath context.cache id model.selection
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
