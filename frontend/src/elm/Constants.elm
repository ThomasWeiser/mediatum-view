module Constants exposing
    ( apiUrl, graphqlOperationNamePrefix
    , incrementLimitOnLoadMore
    , maxAttributeLengthInListingView
    , externalServerUrls
    , filetypes
    )

{-| Configurable values

@docs apiUrl, graphqlOperationNamePrefix
@docs incrementLimitOnLoadMore
@docs maxAttributeLengthInListingView
@docs externalServerUrls
@docs filetypes

-}

import Types.Id as Id


{-| Endpoint for backend's GraphQL service.
-}
apiUrl : String
apiUrl =
    "/graphql"


{-| A common prefix to use for all GraphQL operation names used by the app.

For operation names see: <https://graphql.org/learn/queries/#operation-name>

-}
graphqlOperationNamePrefix : String
graphqlOperationNamePrefix =
    "mediatumView_"


{-| -}
incrementLimitOnLoadMore : Int -> Int
incrementLimitOnLoadMore limit =
    if limit <= 10 then
        20

    else if limit <= 30 then
        50

    else if limit <= 70 then
        100

    else
        ((limit + 149) // 100) * 100


{-| -}
maxAttributeLengthInListingView : Int
maxAttributeLengthInListingView =
    200


{-| Content like document files and thumbnails are provided by a separate server.

This value is a record of URL building functions to this server.

-}
externalServerUrls :
    { thumbnail : Id.DocumentId -> String
    , presentation : Id.DocumentId -> String
    , item : String -> String
    , documentPermanent : Id.DocumentId -> String
    , bibtex : Id.DocumentId -> String
    , bibtexLogo : String
    , showDocumentPdf : Id.DocumentId -> String
    , downloadDocumentPdf : Id.DocumentId -> String
    , urn : String -> String
    , doi : String -> String
    }
externalServerUrls =
    let
        appendId base id =
            base ++ Id.toString id
    in
    { thumbnail = "https://mediatum.ub.tum.de/thumbs/" |> appendId
    , presentation = "https://mediatum.ub.tum.de/thumb2/" |> appendId
    , item = \itemSpec -> "https://mediatum.ub.tum.de/?item=" ++ itemSpec ++ ".html"
    , documentPermanent = "https://mediatum.ub.tum.de/" |> appendId
    , bibtex = \id -> "https://mediatum.ub.tum.de/export/" ++ Id.toString id ++ "/bibtex"
    , bibtexLogo = "https://mediatum.ub.tum.de/img/bibtex.gif"
    , showDocumentPdf =
        \id ->
            "https://mediatum.ub.tum.de/doc/" ++ Id.toString id ++ "/" ++ Id.toString id ++ ".pdf"
    , downloadDocumentPdf =
        \id ->
            "https://mediatum.ub.tum.de/download/" ++ Id.toString id ++ "/" ++ Id.toString id ++ ".pdf"
    , urn = \urnSpec -> "https://nbn-resolving.de/urn/resolver.pl?" ++ urnSpec
    , doi = \doiSpec -> "https://doi.org/" ++ doiSpec
    }


{-| Identifier used for certain relevant filetypes associated with documents
-}
filetypes :
    { presentation : String
    , document : String
    }
filetypes =
    { presentation = "presentation"
    , document = "document"
    }
