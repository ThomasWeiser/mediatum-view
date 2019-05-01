module Api.Fragments exposing
    ( folder, folderAndSubfolders, folderLineage
    , folderAndSubfolderCounts, folderCount
    , documentResultPage, documentResult, documentByMask
    , graphqlDocumentObjects
    )

{-| Definitions of GraphQL query fragments used in the root queries.


# Fragments on Folder

@docs folder, folderAndSubfolders, folderLineage


# Fragments for Facet Queries

@docs folderAndSubfolderCounts, folderCount


# Fragments for Document Results

@docs documentResultPage, documentResult, documentByMask


# Relay Connection Utility

@docs graphqlDocumentObjects

-}

import Dict
import Document exposing (Document)
import DocumentResult exposing (DocumentResult)
import Folder exposing (Folder, FolderId)
import Graphql.Object
import Graphql.Object.Docset
import Graphql.Object.Document
import Graphql.Object.DocumentResult
import Graphql.Object.DocumentResultPage
import Graphql.Object.DocumentsConnection
import Graphql.Object.DocumentsEdge
import Graphql.Object.Folder
import Graphql.Object.FolderCount
import Graphql.Object.FolderCountsConnection
import Graphql.Object.FoldersConnection
import Graphql.Object.Metadatatype
import Graphql.Object.PageInfo
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Scalar
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode exposing (Decoder)
import List.Nonempty exposing (Nonempty)
import Pagination.Offset.Page
import Pagination.Relay.Connection as Connection


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
folder : SelectionSet Folder Graphql.Object.Folder
folder =
    SelectionSet.succeed Folder.init
        |> SelectionSet.with (Graphql.Object.Folder.id |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with Graphql.Object.Folder.parentId
        |> SelectionSet.with (Graphql.Object.Folder.name |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with (Graphql.Object.Folder.isCollection |> SelectionSet.nonNullOrFail)
        |> SelectionSet.with (Graphql.Object.Folder.numSubfolder |> SelectionSet.nonNullOrFail)


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
folderAndSubfolders : SelectionSet ( Folder, List Folder ) Graphql.Object.Folder
folderAndSubfolders =
    SelectionSet.succeed Tuple.pair
        |> SelectionSet.with folder
        |> SelectionSet.with
            (Graphql.Object.Folder.subfolders identity
                (SelectionSet.succeed identity
                    |> SelectionSet.with (Graphql.Object.FoldersConnection.nodes folder)
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
folderLineage : SelectionSet (Nonempty Folder) Graphql.Object.Folder
folderLineage =
    Graphql.Object.Folder.lineage
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
folderAndSubfolderCounts : SelectionSet Folder.FolderCounts Graphql.Object.Docset
folderAndSubfolderCounts =
    SelectionSet.succeed
        (\pair listOfPairs -> Dict.fromList (pair :: listOfPairs))
        |> SelectionSet.with
            (Graphql.Object.Docset.folderCount
                folderCount
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Docset.subfolderCounts
                identity
                (SelectionSet.succeed identity
                    |> SelectionSet.with
                        (Graphql.Object.FolderCountsConnection.nodes
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
folderCount : SelectionSet ( FolderId, Int ) Graphql.Object.FolderCount
folderCount =
    SelectionSet.succeed Tuple.pair
        |> SelectionSet.with
            (Graphql.Object.FolderCount.folderId
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map Folder.idFromInt
            )
        |> SelectionSet.with
            (Graphql.Object.FolderCount.count
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a DocumentResultPage to get a page of a paginated list of documents.

The page contains a list of document results as well as some pagination-specific data.

The nested documents are rendered according to a named mediaTUM mask.

_GraphQL notation:_

    fragment documentResultPage on DocumentResultPage {
        offset
        hasNextPage
        content {
            ...documentResult
        }
    }

-}
documentResultPage :
    String
    -> SelectionSet (Pagination.Offset.Page.Page DocumentResult) Graphql.Object.DocumentResultPage
documentResultPage maskName =
    SelectionSet.succeed Pagination.Offset.Page.Page
        |> SelectionSet.with
            (Graphql.Object.DocumentResultPage.offset
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResultPage.hasNextPage
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResultPage.content
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
documentResult : String -> SelectionSet DocumentResult Graphql.Object.DocumentResult
documentResult maskName =
    SelectionSet.succeed DocumentResult.init
        |> SelectionSet.with
            (Graphql.Object.DocumentResult.number
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResult.distance
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.DocumentResult.document
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
documentByMask : String -> SelectionSet Document Graphql.Object.Document
documentByMask maskName =
    SelectionSet.succeed Document.init
        |> SelectionSet.with
            (Graphql.Object.Document.id
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Document.metadatatype
                (SelectionSet.succeed identity
                    |> SelectionSet.with
                        (Graphql.Object.Metadatatype.longname
                            |> SelectionSet.nonNullOrFail
                        )
                )
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Document.name
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Graphql.Object.Document.valuesByMask
                (\optionals ->
                    { optionals
                        | maskName = Present maskName
                    }
                )
                |> SelectionSet.map mapJsonToAttributes
            )


{-| Decode a JSON string returned from a query that denotes the mata-values of a document.
-}
mapJsonToAttributes : Maybe Graphql.Scalar.Json -> List Document.Attribute
mapJsonToAttributes maybeJson =
    case maybeJson of
        Nothing ->
            []

        Just (Graphql.Scalar.Json str) ->
            Result.withDefault [] <|
                Json.Decode.decodeString decoderAttributeList str


decoderAttributeList : Decoder (List Document.Attribute)
decoderAttributeList =
    Json.Decode.oneOf
        [ Json.Decode.null []
        , Json.Decode.list <|
            Json.Decode.map4 Document.Attribute
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
graphqlDocumentObjects : Connection.GraphqlObjects {} Graphql.Object.DocumentsConnection Graphql.Object.DocumentsEdge Graphql.Object.Document Graphql.Object.PageInfo Graphql.Scalar.Cursor Document
graphqlDocumentObjects =
    { totalCount = Graphql.Object.DocumentsConnection.totalCount
    , pageInfo = Graphql.Object.DocumentsConnection.pageInfo
    , edges = Graphql.Object.DocumentsConnection.edges
    , cursor = Graphql.Object.DocumentsEdge.cursor
    , node = Graphql.Object.DocumentsEdge.node
    , hasNextPage = Graphql.Object.PageInfo.hasNextPage
    , hasPreviousPage = Graphql.Object.PageInfo.hasPreviousPage
    }
