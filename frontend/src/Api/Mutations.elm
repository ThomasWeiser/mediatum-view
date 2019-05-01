module Api.Mutations exposing (updateDocumentAttribute)

{-| Definitions of all specific GraphQL mutation requests needed in the application.


# GraphQL Mutation Definitions

@docs updateDocumentAttribute

-}

import Api.Fragments
import Document exposing (Document, DocumentId)
import Graphql.Mutation
import Graphql.Object.UpdateDocumentAttributePayload
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Maybe.Extra


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
            ...documentByMask
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
                        (Api.Fragments.documentByMask "nodebig")
                    )
            )
        )
