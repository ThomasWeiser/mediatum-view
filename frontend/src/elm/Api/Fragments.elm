module Api.Fragments exposing
    ( translation, ftsAspectConfig, facetAspectConfig, masksPurposeConfig
    , folder, folderAndSubfolders, folderLineageFolders
    , folderAndSubfolderCounts, folderCount, facetByAspect
    , documentsPage, documentResult, documentByMask, documentResidence
    , nonNullElementsOfMaybeListOrFail
    )

{-| Definitions of GraphQL subqueries used in the toplevel queries.


# Fragments of Config

@docs translation, ftsAspectConfig, facetAspectConfig, masksPurposeConfig


# Fragments on Folder

@docs folder, folderAndSubfolders, folderLineageFolders


# Fragments for Facet Queries

@docs folderAndSubfolderCounts, folderCount, facetByAspect


# Fragments for Document Results

@docs documentsPage, documentResult, documentByMask, documentResidence


# Utils for SelectionSet

@docs nonNullElementsOfMaybeListOrFail

-}

import Entities.Document as Document exposing (Document, File, Files)
import Entities.DocumentResults exposing (DocumentResult, DocumentsPage)
import Entities.Folder as Folder exposing (Folder, LineageFolders)
import Entities.FolderCounts as FolderCounts exposing (FolderCounts)
import Entities.Markup
import Entities.Residence exposing (Residence)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode exposing (Decoder)
import List.Nonempty
import Maybe.Extra
import Mediatum.Object
import Mediatum.Object.Docset
import Mediatum.Object.Document
import Mediatum.Object.DocumentFromSearch
import Mediatum.Object.DocumentResult
import Mediatum.Object.DocumentResultPage
import Mediatum.Object.FacetAspectConfig
import Mediatum.Object.FacetValue
import Mediatum.Object.FacetValuesConnection
import Mediatum.Object.File
import Mediatum.Object.Folder
import Mediatum.Object.FolderCount
import Mediatum.Object.FolderCountsConnection
import Mediatum.Object.FoldersConnection
import Mediatum.Object.FtsAspectConfig
import Mediatum.Object.MasksPurposeConfig
import Mediatum.Object.Metadatatype
import Mediatum.Object.Translation
import Mediatum.Scalar
import String.Extra
import Types exposing (FolderDisplay(..), WindowPage)
import Types.Aspect as Aspect exposing (Aspect)
import Types.Config.FacetAspectConfig exposing (FacetAspectConfig)
import Types.Config.FtsAspectConfig exposing (FtsAspectConfig)
import Types.Config.MasksConfig exposing (MasksPurposeServerConfig)
import Types.FacetValue exposing (FacetValue, FacetValues)
import Types.Id as Id exposing (FolderId, LineageIds)
import Types.Localization as Localization
import Types.SearchTerm exposing (SearchTerm)
import Utils


{-| Selection set on a FtsAspectConfig to get a FtsAspect

_GraphQL notation:_

    fragment ftsAspectConfig on FtsAspectConfig
        aspect
        label {
            ...translation
        }
    }

-}
ftsAspectConfig : SelectionSet FtsAspectConfig Mediatum.Object.FtsAspectConfig
ftsAspectConfig =
    SelectionSet.succeed FtsAspectConfig
        |> SelectionSet.with
            (Mediatum.Object.FtsAspectConfig.aspect
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map Aspect.fromString
            )
        |> SelectionSet.with
            (Mediatum.Object.FtsAspectConfig.label
                translation
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a FacetAspectConfig to get a FacetAspect

_GraphQL notation:_

    fragment facetAspectConfig on FacetAspectConfig
        aspect
        label {
            ...translation
        }
    }

-}
facetAspectConfig : SelectionSet FacetAspectConfig Mediatum.Object.FacetAspectConfig
facetAspectConfig =
    SelectionSet.succeed FacetAspectConfig
        |> SelectionSet.with
            (Mediatum.Object.FacetAspectConfig.aspect
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.map Aspect.fromString
            )
        |> SelectionSet.with
            (Mediatum.Object.FacetAspectConfig.label
                translation
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a MasksPurposeConfig to get a MasksPurposeServerConfig

_GraphQL notation:_

    fragment masksPurposeConfig on MasksPurposeConfig
        purpose
        maskeNames {
            ...translation
        }
    }

-}
masksPurposeConfig : SelectionSet MasksPurposeServerConfig Mediatum.Object.MasksPurposeConfig
masksPurposeConfig =
    SelectionSet.succeed
        MasksPurposeServerConfig
        |> SelectionSet.with
            (Mediatum.Object.MasksPurposeConfig.purpose
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.MasksPurposeConfig.maskNames
                translation
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a FolderCount to get the count of the selected documents within the folder.

_GraphQL notation:_

    fragment translation on Translation
        en
        de
    }

-}
translation : SelectionSet Localization.Translations Mediatum.Object.Translation
translation =
    SelectionSet.succeed Localization.Translations
        |> SelectionSet.with
            (Mediatum.Object.Translation.en
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.Translation.de
                |> SelectionSet.nonNullOrFail
            )


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
            (Mediatum.Object.Folder.hasSubfolder
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


{-| Selection set on a Docset to get the top values of a facet given by an aspect name.

_GraphQL notation:_

    fragment facetByAspect on Docset {
        facetByAspect(aspectName: aspect, first: limit) {
            ...facetValues
        }
    }

-}
facetByAspect : Aspect -> Int -> SelectionSet FacetValues Mediatum.Object.Docset
facetByAspect aspect limit =
    SelectionSet.succeed identity
        |> SelectionSet.with
            (Mediatum.Object.Docset.facetByAspect
                (\optionals ->
                    { optionals
                        | first = Present limit
                    }
                )
                { aspectName = Aspect.toString aspect }
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
    -> Maybe SearchTerm
    -> SelectionSet DocumentsPage Mediatum.Object.DocumentResultPage
documentsPage maskName maybeSearchTerm =
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
                (documentResult maskName maybeSearchTerm)
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
documentResult : String -> Maybe SearchTerm -> SelectionSet DocumentResult Mediatum.Object.DocumentResult
documentResult maskName maybeSearchTerm =
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
                (documentByMask maskName maybeSearchTerm False)
                |> SelectionSet.nonNullOrFail
            )


{-| Selection set on a Document to get the basic properties of the document
together with the document's attributes selected by a named mediaTUM mask.

Optionally a SearchTerm maybe highlighted within the attributes.

_GraphQL notation:_

    fragment documentByMask on Document {
        id
        metadatatype {
            longname
        }
        name
        valuesByMask(maskName: maskName, highlightSearchTerm: optionalHighlightSearchTerm)
    }

-}
documentByMask : String -> Maybe SearchTerm -> Bool -> SelectionSet Document Mediatum.Object.Document
documentByMask maskName maybeSearchTerm withFiles =
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
                (\optionals ->
                    { optionals
                        | highlightSearchTerm =
                            maybeSearchTerm
                                |> Maybe.Extra.unwrap
                                    Absent
                                    (Types.SearchTerm.toString >> Present)
                    }
                )
                { maskName = maskName }
                |> SelectionSet.map mapJsonToAttributes
            )
        |> (case maybeSearchTerm of
                Just searchTerm ->
                    SelectionSet.with
                        (Mediatum.Object.Document.fromSearch
                            { text = Types.SearchTerm.toString searchTerm }
                            (SelectionSet.succeed Document.SearchMatching
                                |> SelectionSet.with
                                    (Mediatum.Object.DocumentFromSearch.fulltextMatching
                                        |> SelectionSet.nonNullOrFail
                                    )
                                |> SelectionSet.with
                                    (Mediatum.Object.DocumentFromSearch.attributesMatching
                                        |> SelectionSet.nonNullOrFail
                                    )
                            )
                            |> SelectionSet.nonNullOrFail
                            |> SelectionSet.map Just
                        )

                Nothing ->
                    SelectionSet.hardcoded Nothing
           )
        |> (if withFiles then
                SelectionSet.with
                    (Mediatum.Object.Document.files
                        documentFile
                        |> nonNullElementsOfMaybeListOrFail
                    )

            else
                SelectionSet.hardcoded Nothing
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


{-| -}
documentFile : SelectionSet File Mediatum.Object.File
documentFile =
    SelectionSet.succeed File
        |> SelectionSet.with
            (Mediatum.Object.File.filetype
                |> SelectionSet.nonNullOrFail
            )
        |> SelectionSet.with
            (Mediatum.Object.File.mimetype
                |> SelectionSet.nonNullOrFail
            )


{-| Similar to Graphql.SelectionSet.nonNullElementsOrFail, but the list is and remains wrapped in a Maybe.
-}
nonNullElementsOfMaybeListOrFail :
    SelectionSet (Maybe (List (Maybe decodesTo))) scope
    -> SelectionSet (Maybe (List decodesTo)) scope
nonNullElementsOfMaybeListOrFail =
    SelectionSet.mapOrFail
        (\maybeListMaybe ->
            case
                Maybe.map Maybe.Extra.combine maybeListMaybe
            of
                Nothing ->
                    Ok Nothing

                Just (Just l) ->
                    Ok (Just l)

                Just Nothing ->
                    Err "Expected only non-null list elements but found a null."
        )


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
                (Json.Decode.field "value"
                    (Json.Decode.string
                        |> Json.Decode.map
                            (Entities.Markup.parse
                                (Entities.Markup.SpanClass "unparsable")
                            )
                        |> Json.Decode.map
                            Entities.Markup.fixSpacesAfterSeparators
                        |> Json.Decode.maybe
                    )
                )
        ]
