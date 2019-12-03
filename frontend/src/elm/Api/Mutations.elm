module Api.Mutations exposing (updateDocumentAttribute)

{-| Definitions of all specific GraphQL mutation requests needed in the application.


# GraphQL Mutation Definitions

@docs updateDocumentAttribute

-}

import Api.Fragments
import Entities.Document exposing (Document)
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Maybe.Extra
import Mediatum.Mutation
import Mediatum.Object.UpdateDocumentAttributePayload
import Types.Id as Id exposing (DocumentId)


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
        (Mediatum.Mutation.updateDocumentAttribute
            { input =
                { clientMutationId = Absent
                , id = Present (Id.toInt documentId)
                , key = Present key
                , value = Present value
                }
            }
            (SelectionSet.succeed identity
                |> SelectionSet.with
                    (Mediatum.Object.UpdateDocumentAttributePayload.document
                        (Api.Fragments.documentByMask "nodebig")
                    )
            )
        )
