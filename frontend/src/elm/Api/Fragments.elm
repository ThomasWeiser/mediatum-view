module Api.Fragments exposing
    ( folder, folderAndSubfolders, folderLineageFolders
    , folderAndSubfolderCounts, folderCount, facetByKey
    , documentsPage, documentResult, documentByMask, documentResidence
    , graphqlDocumentObjects
    )

{-| Definitions of GraphQL subqueries used in the toplevel queries.


# Fragments on Folder

@docs folder, folderAndSubfolders, folderLineageFolders


# Fragments for Facet Queries

@docs folderAndSubfolderCounts, folderCount, facetByKey


# Fragments for Document Results

@docs documentsPage, documentResult, documentByMask, documentResidence


# Relay Connection Utility

@docs graphqlDocumentObjects

-}

import Entities.Document as Document exposing (Document)
import Entities.DocumentResults exposing (DocumentResult, DocumentsPage)
import Entities.Folder as Folder exposing (Folder, LineageFolders)
import Entities.FolderCounts as FolderCounts exposing (FolderCounts)
import Entities.Residence exposing (Residence)
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
import Mediatum.Object.FacetValue
import Mediatum.Object.FacetValuesConnection
import Mediatum.Object.Folder
import Mediatum.Object.FolderCount
import Mediatum.Object.FolderCountsConnection
import Mediatum.Object.FoldersConnection
import Mediatum.Object.Metadatatype
import Mediatum.Object.PageInfo
import Mediatum.Scalar
import Pagination.Relay.Connection as Connection
import String.Extra
import Types exposing (FolderDisplay(..), WindowPage)
import Types.Facet exposing (FacetValue, FacetValues)
import Types.Id as Id exposing (FolderId, LineageIds)
import Utils


{-| Selection set on a [`Folder`](Entities-Folder) to get basic properties of the folder.

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
                |> SelectionSet.map Id.fromInt
            )
        |> SelectionSet.with
            (Mediatum.Object.Folder.parentId
                |> SelectionSet.map (Maybe.map Id.fromInt)
            )
        |> SelectionSet.with
            (Mediatum.Object.Folder.name
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.Folder.isCollection
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map
                    (Utils.ifElse DisplayAsCollection DisplayAsDirectory)
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
starting with the given folder up to a root folder of the hierarchy.

_GraphQL notation:_

    fragment folderLineage on Folder {
        lineage {
            ...folder
        }
    }

-}
folderLineageFolders : SelectionSet LineageFolders Mediatum.Object.Folder
folderLineageFolders =
    Mediatum.Object.Folder.lineage
        folder
        |> SelectionSet.nonNullOrFail
        |> SelectionSet.nonNullElementsOrFail
        |> SelectionSet.mapOrFail
            (List.Nonempty.fromList
                >> Result.fromMaybe "Lineage needs at least one folder"
            )


{-| Selection set on a folder to get the ids of the lineage of that folder.

The lineage is the non-emtpy list of folder ids representing the path
starting with the given folder up to a root folder of the hierarchy.

_GraphQL notation:_

    fragment folderLineage on Folder {
        lineage {
            id
        }
    }

-}
folderLineageIds : SelectionSet LineageIds Mediatum.Object.Folder
folderLineageIds =
    Mediatum.Object.Folder.lineage
        (Mediatum.Object.Folder.id
            |> SelectionSet.nonNullOrFail
            |> SelectionSet.map Id.fromInt
        )
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
            FolderCounts.fromList
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

    fragment folderCount on FolderCount
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
                |> SelectionSet.map Id.fromInt
            )
        |> SelectionSet.with
            (Mediatum.Object.FolderCount.count
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a Docset to get the top values of a facet given by an attribute key.

_GraphQL notation:_

    fragment facetByKey on Docset {
        facetByKey(key: key, first: limit) {
            ...facetValues
        }
    }

-}
facetByKey : String -> Int -> SelectionSet FacetValues Mediatum.Object.Docset
facetByKey key limit =
    SelectionSet.succeed identity
        |> SelectionSet.with
            (Mediatum.Object.Docset.facetByKey
                (\optionals ->
                    { optionals
                        | first = Present limit
                    }
                )
                { key = key }
                facetValues
            )


{-| Selection set on the nodes of a list of FacetValue.

_GraphQL notation:_

    fragment facetValues on FacetValuesConnection {
        nodes {
            ...facetValue
        }
    }

-}
facetValues : SelectionSet FacetValues Mediatum.Object.FacetValuesConnection
facetValues =
    SelectionSet.succeed identity
        |> SelectionSet.with
            (Mediatum.Object.FacetValuesConnection.nodes
                facetValue
            )


{-| Selection set on a FacetValue to get the value and the count of a facet's figure.

_GraphQL notation:_

    fragment facetValue on FacetValue
        value
        count
    }

-}
facetValue : SelectionSet FacetValue Mediatum.Object.FacetValue
facetValue =
    SelectionSet.succeed FacetValue
        |> SelectionSet.with
            (Mediatum.Object.FacetValue.value
                |> SelectionSet.withDefault ""
                |> SelectionSet.map
                    (String.Extra.nonEmpty >> Maybe.withDefault "")
            )
        |> SelectionSet.with
            (Mediatum.Object.FacetValue.count
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
    SelectionSet.succeed Entities.DocumentResults.DocumentResult
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
                |> SelectionSet.map Id.fromInt
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
                { maskName = maskName }
                |> SelectionSet.map mapJsonToAttributes
            )


{-| Selection set on a Document to get the residence of the document,
i.e. a list of folders with their lineage in which the document appears.

_GraphQL notation:_

    fragment documentResidence on Document {
        folder {
            folderLineageIds
        }
    }

-}
documentResidence : SelectionSet Residence Mediatum.Object.Document
documentResidence =
    Mediatum.Object.Document.folders
        folderLineageIds
        |> SelectionSet.nonNullOrFail
        |> SelectionSet.nonNullElementsOrFail


{-| Decode a JSON string returned from a query that denotes the mata-values of a document.
-}
mapJsonToAttributes : Maybe Mediatum.Scalar.Json -> List Document.Attribute
mapJsonToAttributes maybeJson =
    case maybeJson of
        Nothing ->
            []

        Just (Mediatum.Scalar.Json str) ->
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
