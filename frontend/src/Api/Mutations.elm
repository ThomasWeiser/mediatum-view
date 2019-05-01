module Api.Mutations exposing (updateDocumentAttribute)

{-| Definitions of all specific GraphQL mutation requests needed for the application.


# GraphQL Mutation Definitions

@docs updateDocumentAttribute

-}

import Api.Fragments
import Dict
import Document exposing (Document, DocumentId)
import DocumentResult exposing (DocumentResult)
import Folder exposing (Folder, FolderCounts, FolderId)
import GenericNode exposing (GenericNode)
import Graphql.Extra
import Graphql.Http
import Graphql.Mutation
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
import Graphql.Object.GenericNode
import Graphql.Object.Metadatatype
import Graphql.Object.PageInfo
import Graphql.Object.UpdateDocumentAttributePayload
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Query
import Graphql.Scalar
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode exposing (Decoder)
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Pagination.Offset.Page
import Pagination.Relay.Connection as Connection
import Pagination.Relay.Page
import Pagination.Relay.Pagination
import Query exposing (Query)
import Query.Attribute


{-| Set an attribute of a document selected by a mediaTUM id.
Returns the new document details based on the mediaTUM mask "nodebig".

_GraphQL notation:_

    mutation {
        updateDocumentAttribute(
            input: {
                id: $idOfTheDocument
                key: $keyOfTheAttribute
                value: $desiredValueOfTheAttribute
            }
        ) {
            ...documentNode
        }
    }

-}
updateDocumentAttribute :
    DocumentId
    -> String
    -> String
    -> SelectionSet (Maybe Document) Graphql.Operation.RootMutation
updateDocumentAttribute documentId key value =
    SelectionSet.map Maybe.Extra.join
        (Graphql.Mutation.updateDocumentAttribute
            { input =
                { clientMutationId = Absent
                , id = Present (Document.idToInt documentId)
                , key = Present key
                , value = Present value
                }
            }
            (SelectionSet.succeed identity
                |> SelectionSet.with
                    (Graphql.Object.UpdateDocumentAttributePayload.document
                        (Api.Fragments.documentNode "nodebig")
                    )
            )
        )
