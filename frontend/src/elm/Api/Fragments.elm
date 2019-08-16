module Api.Fragments exposing
    ( folder, folderAndSubfolders, folderLineage
    , folderAndSubfolderCounts, folderCount
    , documentsPage, documentResult, documentByMask
    , graphqlDocumentObjects
    )

{-| Definitions of GraphQL query fragments used in the root queries.


# Fragments on Folder

@docs folder, folderAndSubfolders, folderLineage


# Fragments for Facet Queries

@docs folderAndSubfolderCounts, folderCount


# Fragments for Document Results

@docs documentsPage, documentResult, documentByMask


# Relay Connection Utility

@docs graphqlDocumentObjects

-}

import Data.Types exposing (..)
import Data.Utils
import Document
import DocumentResult
import Folder
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode exposing (Decoder)
import List.Nonempty exposing (Nonempty)
import Mediatum.Object
import Mediatum.Object.Docset
import Mediatum.Object.Document
import Mediatum.Object.DocumentResult
import Mediatum.Object.DocumentResultPage
import Mediatum.Object.DocumentsConnection
import Mediatum.Object.DocumentsEdge
import Mediatum.Object.Folder
import Mediatum.Object.FolderCount
import Mediatum.Object.FolderCountsConnection
import Mediatum.Object.FoldersConnection
import Mediatum.Object.Metadatatype
import Mediatum.Object.PageInfo
import Mediatum.Scalar
import Pagination.Offset.Page
import Pagination.Relay.Connection as Connection
import Sort.Dict
import Utils


{-| Selection set on a Folder to get basic properties of the folder.

_GraphQL notation:_

    fragment folder on Folder {
        id
        parentId
        name
        isCollection
        numSubfolder
    }

-}
folder : SelectionSet Folder Mediatum.Object.Folder
folder =
    SelectionSet.succeed Folder.init
        |> SelectionSet.with
            (Mediatum.Object.Folder.id
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map folderIdFromInt
            )
        |> SelectionSet.with
            (Mediatum.Object.Folder.parentId
                |> SelectionSet.map (Maybe.map folderIdFromInt)
            )
        |> SelectionSet.with
            (Mediatum.Object.Folder.name
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.Folder.isCollection
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map
                    (Utils.ifElse FolderIsCollection FolderIsDirectory)
            )
        |> SelectionSet.with
            (Mediatum.Object.Folder.numSubfolder
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a folder to get the basic properties of that folder and of its sub-folders.

_GraphQL notation:_

    fragment folderAndSubfolders on Folder {
        ...folder
        subfolders {
            nodes {
                ...folder
            }
        }
    }

-}
folderAndSubfolders : SelectionSet ( Folder, List Folder ) Mediatum.Object.Folder
folderAndSubfolders =
    SelectionSet.succeed Tuple.pair
        |> SelectionSet.with folder
        |> SelectionSet.with
            (Mediatum.Object.Folder.subfolders identity
                (SelectionSet.succeed identity
                    |> SelectionSet.with (Mediatum.Object.FoldersConnection.nodes folder)
                )
            )


{-| Selection set on a folder to get the lineage of that folder.

The lineage is the non-emtpy list of folders representing the path
from the given folder up to a root folder of the hierarchy.

_GraphQL notation:_

    fragment folderLineage on Folder {
        lineage {
            ...folder
        }
    }

-}
folderLineage : SelectionSet (Nonempty Folder) Mediatum.Object.Folder
folderLineage =
    Mediatum.Object.Folder.lineage
        folder
        |> SelectionSet.nonNullOrFail
        |> SelectionSet.nonNullElementsOrFail
        |> SelectionSet.mapOrFail
            (List.Nonempty.fromList
                >> Result.fromMaybe "Lineage needs at least one folder"
            )


{-| Selection set on a Docset to get the counts of documents within.

_GraphQL notation:_

    fragment folderAndSubfolderCounts on Docset {
        folderCount {
            ...folderCount
        }
        subfolderCounts {
            nodes {
                ...folderCount
            }
        }
    }

-}
folderAndSubfolderCounts : SelectionSet FolderCounts Mediatum.Object.Docset
folderAndSubfolderCounts =
    SelectionSet.succeed
        (\pair listOfPairs ->
            Data.Utils.folderCountsFromList
                (pair :: listOfPairs)
        )
        |> SelectionSet.with
            (Mediatum.Object.Docset.folderCount
                folderCount
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.Docset.subfolderCounts
                identity
                (SelectionSet.succeed identity
                    |> SelectionSet.with
                        (Mediatum.Object.FolderCountsConnection.nodes
                            folderCount
                        )
                )
            )


{-| Selection set on a FolderCount to get the count of the selected documents within the folder.

_GraphQL notation:_

    fragment folderCount on FolderCount Facet Queries
        count
        folderId
    }

-}
folderCount : SelectionSet ( FolderId, Int ) Mediatum.Object.FolderCount
folderCount =
    SelectionSet.succeed Tuple.pair
        |> SelectionSet.with
            (Mediatum.Object.FolderCount.folderId
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map folderIdFromInt
            )
        |> SelectionSet.with
            (Mediatum.Object.FolderCount.count
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a DocumentResultPage to get a page of a paginated list of documents.

The page contains a list of document results as well as some pagination-specific data.

The nested documents are rendered according to a named mediaTUM mask.

_GraphQL notation:_

    fragment documentsPage on DocumentResultPage {
        offset
        hasNextPage
        content {
            ...documentResult
        }
    }

-}
documentsPage :
    String
    -> SelectionSet DocumentsPage Mediatum.Object.DocumentResultPage
documentsPage maskName =
    SelectionSet.succeed WindowPage
        |> SelectionSet.with
            (Mediatum.Object.DocumentResultPage.offset
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.DocumentResultPage.hasNextPage
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.DocumentResultPage.content
                (documentResult maskName)
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.nonNullElementsOrFail
            )


{-| Selection set on a DocumentResult to get the document data and additional search result characterization
(position of the result, ranking of the result).

The nested document is rendered according to a named mediaTUM mask.

_GraphQL notation:_

    fragment documentResult on DocumentResult {
        number
        distance
        ...documentByMask
    }

-}
documentResult : String -> SelectionSet DocumentResult Mediatum.Object.DocumentResult
documentResult maskName =
    SelectionSet.succeed DocumentResult.init
        |> SelectionSet.with
            (Mediatum.Object.DocumentResult.number
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.DocumentResult.distance
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.DocumentResult.document
                (documentByMask maskName)
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a Document to get the basic properties of the document
together with the document's attributes selected by a named mediaTUM mask.

_GraphQL notation:_

    fragment documentByMask on Document {
        id
        metadatatype {
            longname
        }
        name
        valuesByMask(maskName: maskName)
    }

-}
documentByMask : String -> SelectionSet Document Mediatum.Object.Document
documentByMask maskName =
    SelectionSet.succeed Document.init
        |> SelectionSet.with
            (Mediatum.Object.Document.id
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map documentIdFromInt
            )
        |> SelectionSet.with
            (Mediatum.Object.Document.metadatatype
                (SelectionSet.succeed identity
                    |> SelectionSet.with
                        (Mediatum.Object.Metadatatype.longname
                            |> SelectionSet.nonNullOrFail
                        )
                )
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.Document.name
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.Document.valuesByMask
                (\optionals ->
                    { optionals
                        | maskName = Present maskName
                    }
                )
                |> SelectionSet.map mapJsonToAttributes
            )


{-| Decode a JSON string returned from a query that denotes the mata-values of a document.
-}
mapJsonToAttributes : Maybe Mediatum.Scalar.Json -> List DocumentAttribute
mapJsonToAttributes maybeJson =
    case maybeJson of
        Nothing ->
            []

        Just (Mediatum.Scalar.Json str) ->
            Result.withDefault [] <|
                Json.Decode.decodeString decoderAttributeList str


decoderAttributeList : Decoder (List DocumentAttribute)
decoderAttributeList =
    Json.Decode.oneOf
        [ Json.Decode.null []
        , Json.Decode.list <|
            Json.Decode.map4 DocumentAttribute
                (Json.Decode.field "field" Json.Decode.string)
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "width" Json.Decode.int)
                (Json.Decode.field "value" (Json.Decode.maybe Json.Decode.string))
        ]


{-| Configuration object for abstracting Relay pagination functions
referring to a list of documents.

It contains all the selection set functions needed for building
queries according to the Relay pagination specification.

-}
graphqlDocumentObjects : Connection.GraphqlObjects {} Mediatum.Object.DocumentsConnection Mediatum.Object.DocumentsEdge Mediatum.Object.Document Mediatum.Object.PageInfo Mediatum.Scalar.Cursor Document
graphqlDocumentObjects =
    { totalCount = Mediatum.Object.DocumentsConnection.totalCount
    , pageInfo = Mediatum.Object.DocumentsConnection.pageInfo
    , edges = Mediatum.Object.DocumentsConnection.edges
    , cursor = Mediatum.Object.DocumentsEdge.cursor
    , node = Mediatum.Object.DocumentsEdge.node
    , hasNextPage = Mediatum.Object.PageInfo.hasNextPage
    , hasPreviousPage = Mediatum.Object.PageInfo.hasPreviousPage
    }
