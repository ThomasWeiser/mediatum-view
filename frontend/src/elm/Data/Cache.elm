module Data.Cache exposing (ApiData, Model, Need(..), Needs)

import Api
import Api.Queries
import Basics.Extra
import Data.Types exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import RemoteData exposing (RemoteData(..))


type alias ApiData a =
    RemoteData Api.Error a


type alias Model =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Dict FolderId (ApiData Folder)
    , subfolderIds : Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Dict Int (ApiData NodeType)
    , folderCounts : Dict Selection (ApiData (Dict FolderId Int))
    , docListPages : Dict Selection (Dict Window (ApiData DocumentsPage))
    , documents : Dict DocumentId (ApiData Document)
    }


type alias Needs =
    -- Poss. use a different aggregation abstraction,
    -- like a recursive Need type or a Set
    List Need


type Need
    = NeedRootFolderIds
    | NeedSubfolders (List FolderId)



-- TODO: | NeedNodeType Int
-- TODO: | NeedFolderCounts Selection
-- TODO: | NeedDocListPage Selection Window
-- TODO: | NeedDocument DocumentId


type Msg
    = ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))
    | ApiResponseSubfolder (List FolderId) (Api.Response (List Folder))


requestNeeds : Needs -> Model -> ( Model, Cmd Msg )
requestNeeds needs model =
    List.Extra.mapAccuml
        (Basics.Extra.flip requestNeed)
        model
        needs
        |> Tuple.mapSecond Cmd.batch


requestNeed : Need -> Model -> ( Model, Cmd Msg )
requestNeed need model =
    case need of
        NeedRootFolderIds ->
            case model.rootFolderIds of
                NotAsked ->
                    ( { model
                        | rootFolderIds = Loading
                      }
                    , Api.sendQueryRequest
                        ApiResponseToplevelFolder
                        Api.Queries.toplevelFolder
                    )

                _ ->
                    ( model, Cmd.none )

        NeedSubfolders parentIds ->
            let
                parentIdsWithUnknownChildren =
                    List.filter
                        (\parentId ->
                            (Dict.get parentId model.subfolderIds |> Maybe.withDefault NotAsked)
                                == NotAsked
                        )
                        parentIds
            in
            if List.isEmpty parentIdsWithUnknownChildren then
                ( model, Cmd.none )

            else
                ( { model
                    | subfolderIds =
                        List.foldl
                            (\parentId subfolderIds ->
                                Dict.insert parentId Loading subfolderIds
                            )
                            model.subfolderIds
                            parentIdsWithUnknownChildren
                  }
                , Api.sendQueryRequest
                    (ApiResponseSubfolder parentIds)
                    (Api.Queries.subfolder parentIdsWithUnknownChildren)
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponseToplevelFolder (Ok listOfRootFoldersWithSubfolders) ->
            ( let
                allNewFolders =
                    listOfRootFoldersWithSubfolders
                        |> List.map (Basics.Extra.uncurry (::))
                        |> List.concat
              in
              { model
                | rootFolderIds =
                    Success
                        (List.map (Tuple.first >> .id) listOfRootFoldersWithSubfolders)
              }
                |> insertAsFolders allNewFolders
                |> insertAsSubfolderIds allNewFolders
            , Cmd.none
            )

        ApiResponseToplevelFolder (Err error) ->
            ( { model
                | rootFolderIds = Failure error
              }
            , Cmd.none
            )

        ApiResponseSubfolder _ (Ok listOfSubfolders) ->
            ( model
                |> insertAsFolders listOfSubfolders
                |> insertAsSubfolderIds listOfSubfolders
            , Cmd.none
            )

        ApiResponseSubfolder parentIds (Err error) ->
            ( { model
                | subfolderIds =
                    List.foldl
                        (\parentId subfolderIds ->
                            Dict.insert parentId (Failure error) subfolderIds
                        )
                        model.subfolderIds
                        parentIds
              }
            , Cmd.none
            )


insertAsFolders : List Folder -> Model -> Model
insertAsFolders listOfNewFolders model =
    { model
        | folders =
            List.foldl
                (\folder ->
                    Dict.insert folder.id (Success folder)
                )
                model.folders
                listOfNewFolders
    }


insertAsSubfolderIds : List Folder -> Model -> Model
insertAsSubfolderIds allSubfoldersOfSomeNewParents model =
    { model
        | subfolderIds =
            Dict.Extra.filterGroupBy
                .parent
                allSubfoldersOfSomeNewParents
                |> Dict.toList
                |> List.foldl
                    (\( parentId, subfolders ) ->
                        Dict.insert
                            parentId
                            (Success
                                (List.map .id subfolders)
                            )
                    )
                    model.subfolderIds
    }
