module Types exposing
    ( FolderDisplay(..)
    , NodeType(..)
    , Window
    , WindowPage, sectionOfWindowPage
    , DocumentIdFromSearch
    , orderingWindow, orderingDocumentIdFromSearch
    )

{-| Some general types used throughout the application.

Some more types, especially those that come with accompanying functions, are defined in sub-modules of this module.

Note that certain other types, which are entities representing query results, are defined in sub-modules of `Entities`.

@docs FolderDisplay
@docs NodeType
@docs Window
@docs WindowPage, sectionOfWindowPage
@docs DocumentIdFromSearch

@docs orderingWindow, orderingDocumentIdFromSearch

-}

import Basics.Extra
import Ordering exposing (Ordering)
import Types.Id as Id exposing (DocumentId)
import Types.SearchTerm exposing (SearchTerm)
import Utils


{-| In mediaTUM a folder is marked either as a collection (which is displayed showing its dedicated title page)
or as a directory (which is displayed by listing its content).
-}
type FolderDisplay
    = DisplayAsCollection
    | DisplayAsDirectory


{-| Folders, documents and some other data entities are organized as nodes in mediaTUM, which are identified by numbers.
-}
type NodeType
    = NodeIsFolder FolderDisplay
    | NodeIsDocument
    | NodeIsNeither


{-| A window adresses a page of a offset-based pagination scheme, which we use when querying sets of documents.
-}
type alias Window =
    { offset : Int
    , limit : Int
    }


{-| A page of resulting items of an API request using an offset-based pagination scheme.
-}
type alias WindowPage itemModel =
    { offset : Int
    , hasNextPage : Bool
    , content : List itemModel
    }


{-| Get a section of a WindowPage.

The cuttong points of the section are defined by a Window, whose coordinates are relative to the WindowPage.

The resulting WindowPage cannot exceed the original WindowPage.

-}
sectionOfWindowPage : Window -> WindowPage itemModel -> WindowPage itemModel
sectionOfWindowPage window page =
    let
        pageLength =
            List.length page.content

        windowOffset =
            window.offset |> Basics.Extra.atLeast 0

        windowLimit =
            window.limit
                |> Basics.Extra.atLeast 0
                |> Basics.Extra.atMost (window.offset + window.limit)
    in
    { offset = page.offset + (windowOffset |> Basics.Extra.atMost pageLength)
    , hasNextPage = (windowOffset + windowLimit < pageLength) || page.hasNextPage
    , content = page.content |> List.drop windowOffset |> List.take windowLimit
    }


{-| Define an ordering on the type so we can use it as a key in a `Sort.Dict`.
-}
orderingWindow : Ordering Window
orderingWindow =
    Ordering.byField .offset
        |> Ordering.breakTiesWith
            (Ordering.byField .limit)


{-| A type to query a document id, possibly together with a search term for highlight markup.
-}
type alias DocumentIdFromSearch =
    { id : DocumentId
    , search : Maybe SearchTerm
    }


{-| Ordering on the type DocumentIdFromSearch
-}
orderingDocumentIdFromSearch : Ordering DocumentIdFromSearch
orderingDocumentIdFromSearch =
    Ordering.byFieldWith Id.ordering .id
        |> Ordering.breakTiesWith
            (Ordering.byFieldWith
                (Utils.maybeOrdering Types.SearchTerm.ordering)
                .search
            )
