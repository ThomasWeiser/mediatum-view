module Data.Cache exposing
    ( ApiData
    , Model
    , Msg(..)
    , Needs(..)
    , Return(..)
    , dictGetApiData
    , initialModel
    , requestNeeds
    , update
    )

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


type Return
    = NoReturn
    | GotRootFolders


type alias Model =
    { rootFolderIds : ApiData (List FolderId)
    , folders : Dict FolderId (ApiData Folder)
    , subfolderIds : Dict FolderId (ApiData (List FolderId))
    , nodeTypes : Dict Int (ApiData NodeType)
    , folderCounts : Dict Selection (ApiData (Dict FolderId Int))
    , docListPages : Dict Selection (Dict Window (ApiData DocumentsPage))
    , documents : Dict DocumentId (ApiData Document)
    }


type Needs
    = NeedRootFolderIds
    | NeedSubfolders (List FolderId)
    | NeedListOfNeeds (List Needs)


initialModel : Model
initialModel =
    { rootFolderIds = NotAsked
    , folders = Dict.empty
    , subfolderIds = Dict.empty
    , nodeTypes = Dict.empty
    , folderCounts = Dict.empty
    , docListPages = Dict.empty
    , documents = Dict.empty
    }



-- TODO: | NeedNodeType Int
-- TODO: | NeedFolderCounts Selection
-- TODO: | NeedDocListPage Selection Window
-- TODO: | NeedDocument DocumentId


dictGetApiData : Dict comparable (ApiData value) -> comparable -> ApiData value
dictGetApiData dict key =
    Dict.get key dict
        |> Maybe.withDefault RemoteData.NotAsked


type Msg
    = ApiResponseToplevelFolder (Api.Response (List ( Folder, List Folder )))
    | ApiResponseSubfolder (List FolderId) (Api.Response (List Folder))


requestNeeds : Needs -> Model -> ( Model, Cmd Msg )
requestNeeds needs model =
    case needs of
        NeedListOfNeeds listOfNeeds ->
            listOfNeeds
                |> List.Extra.mapAccuml
                    (Basics.Extra.flip requestNeeds)
                    model
                |> Tuple.mapSecond Cmd.batch

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


update : Msg -> Model -> ( Model, Cmd Msg, Return )
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
            , GotRootFolders
            )

        ApiResponseToplevelFolder (Err error) ->
            ( { model
                | rootFolderIds = Failure error
              }
            , Cmd.none
            , NoReturn
            )

        ApiResponseSubfolder _ (Ok listOfSubfolders) ->
            ( model
                |> insertAsFolders listOfSubfolders
                |> insertAsSubfolderIds listOfSubfolders
            , Cmd.none
            , NoReturn
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
            , NoReturn
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
