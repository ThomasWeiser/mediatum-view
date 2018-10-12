module Article
    exposing
        ( Model
        , Msg
        , Return(..)
        , initEmpty
        , initCollectionOrSearch
        , update
        , view
        )

import Html exposing (Html)
import Html.Attributes
import Folder exposing (Folder, FolderCounts)
import Tree
import Article.Empty
import Article.Collection
import Article.Directory
import Article.Fts exposing (SearchType)
import Article.Details
import Document exposing (DocumentId)
import Utils


type alias Model =
    { static : Static
    , content : Content
    }


type alias Static =
    { folder : Folder
    }


type Content
    = EmptyModel Article.Empty.Model
    | CollectionModel Article.Collection.Model
    | DirectoryModel Article.Directory.Model
    | FtsModel Article.Fts.Model
    | DetailsModel Article.Details.Model


type Msg
    = EmptyMsg Article.Empty.Msg
    | CollectionMsg Article.Collection.Msg
    | DirectoryMsg Article.Directory.Msg
    | FtsMsg Article.Fts.Msg
    | DetailsMsg Article.Details.Msg


type Return
    = NoReturn
    | FolderCounts FolderCounts


initEmpty : () -> ( Model, Cmd Msg )
initEmpty _ =
    let
        ( subModel, subCmd ) =
            Article.Empty.init ()
    in
        ( { static = { folder = Folder.dummy }
          , content = EmptyModel subModel
          }
        , Cmd.map EmptyMsg subCmd
        )


initCollectionOrSearch : Folder -> SearchType -> String -> ( Model, Cmd Msg )
initCollectionOrSearch folder searchType searchString =
    let
        static =
            { folder = folder }
    in
        if searchString == "" then
            if folder.isCollection && searchString == "" then
                let
                    ( subModel, subCmd ) =
                        Article.Collection.init ()
                in
                    ( { static = static
                      , content = CollectionModel subModel
                      }
                    , Cmd.map CollectionMsg subCmd
                    )
            else
                let
                    ( subModel, subCmd ) =
                        Article.Directory.init static ()
                in
                    ( { static = static
                      , content = DirectoryModel subModel
                      }
                    , Cmd.map DirectoryMsg subCmd
                    )
        else
            let
                ( subModel, subCmd ) =
                    Article.Fts.init
                        static
                        { searchType = searchType
                        , searchString = searchString
                        }
            in
                ( { static = static
                  , content = FtsModel subModel
                  }
                , Cmd.map FtsMsg subCmd
                )


initDetails : Folder -> DocumentId -> ( Model, Cmd Msg )
initDetails folder id =
    let
        ( subModel, subCmd ) =
            Article.Details.init id
    in
        ( { static = { folder = folder }
          , content = DetailsModel subModel
          }
        , Cmd.map DetailsMsg subCmd
        )


update : Msg -> Model -> ( Model, Cmd Msg, Return )
update msg model =
    case ( msg, model.content ) of
        ( EmptyMsg subMsg, EmptyModel subModel ) ->
            let
                ( subModel1, subCmd ) =
                    Article.Empty.update subMsg subModel
            in
                ( { model | content = EmptyModel subModel1 }
                , Cmd.map EmptyMsg subCmd
                , NoReturn
                )

        ( CollectionMsg subMsg, CollectionModel subModel ) ->
            let
                ( subModel1, subCmd ) =
                    Article.Collection.update subMsg subModel
            in
                ( { model | content = CollectionModel subModel1 }
                , Cmd.map CollectionMsg subCmd
                , NoReturn
                )

        ( DirectoryMsg subMsg, DirectoryModel subModel ) ->
            let
                ( subModel1, subCmd, documentSelection ) =
                    Article.Directory.update
                        subMsg
                        model.static
                        subModel
            in
                case documentSelection of
                    Nothing ->
                        ( { model | content = DirectoryModel subModel1 }
                        , Cmd.map DirectoryMsg subCmd
                        , NoReturn
                        )

                    Just documentId ->
                        initDetails model.static.folder documentId
                            |> Utils.tupleAddThird NoReturn

        ( FtsMsg subMsg, FtsModel subModel ) ->
            let
                ( subModel1, subCmd, subReturn ) =
                    Article.Fts.update
                        subMsg
                        model.static
                        subModel
            in
                case subReturn of
                    Article.Fts.NoReturn ->
                        ( { model | content = FtsModel subModel1 }
                        , Cmd.map FtsMsg subCmd
                        , NoReturn
                        )

                    Article.Fts.FolderCounts folderCounts ->
                        ( model
                        , Cmd.none
                        , FolderCounts folderCounts
                        )

                    Article.Fts.SelectedDocument documentId ->
                        initDetails model.static.folder documentId
                            |> Utils.tupleAddThird NoReturn

        ( DetailsMsg subMsg, DetailsModel subModel ) ->
            let
                ( subModel1, subCmd ) =
                    Article.Details.update subMsg subModel
            in
                ( { model | content = DetailsModel subModel1 }
                , Cmd.map DetailsMsg subCmd
                , NoReturn
                )

        _ ->
            -- Message doesn't match model; shouldn't never happen
            ( model, Cmd.none, NoReturn )


view : Tree.Model -> Model -> Html Msg
view tree model =
    Html.div []
        [ Html.div
            [ Html.Attributes.class "breadcrumbs" ]
            [ Tree.viewBreadcrumbs tree model.static.folder.id ]
        , viewContent model
        ]


viewContent : Model -> Html Msg
viewContent model =
    case model.content of
        EmptyModel subModel ->
            Article.Empty.view subModel
                |> Html.map EmptyMsg

        CollectionModel subModel ->
            Article.Collection.view model.static subModel
                |> Html.map CollectionMsg

        DirectoryModel subModel ->
            Article.Directory.view subModel
                |> Html.map DirectoryMsg

        FtsModel subModel ->
            Article.Fts.view subModel
                |> Html.map FtsMsg

        DetailsModel subModel ->
            Article.Details.view subModel
                |> Html.map DetailsMsg
