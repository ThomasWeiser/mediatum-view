module Entities.PageSequence exposing
    ( PageSequence, init
    , PresentationSegments, presentationSegmentsAll, presentationSegmentsLimited
    , canLoadMore, remoteDataIsSuccess, extent
    , firstDocument, findAdjacentDocuments, findIndex
    , statusOfNeededWindow, requestWindow, updatePageResult
    )

{-| Listings of documents may get queryied in several consecutive pages,
e.g. by a UI button to load more results.

The type `PageSequence` represents such a sequence of pages as it is stored in the cache.

The segmentation of the listing into pages reflects the history of requests to prolong the listing.

@docs PageSequence, init

@docs PresentationSegments, presentationSegmentsAll, presentationSegmentsLimited

@docs canLoadMore, remoteDataIsSuccess, extent

@docs firstDocument, findAdjacentDocuments, findIndex

@docs statusOfNeededWindow, requestWindow, updatePageResult

-}

import Array exposing (Array)
import Entities.DocumentResults exposing (DocumentResult, DocumentsPage)
import List.Extra
import Maybe.Extra
import RemoteData exposing (RemoteData(..))
import Types exposing (Window)
import Types.ApiData exposing (ApiData)
import Types.Id exposing (DocumentId)
import Types.Needs as Needs exposing (Status(..))
import Utils.List


{-| The type `PageSequence` represents a sequence of pages as they are queried from the API
-}
type PageSequence
    = PageSequence InternalSegments Bool


type alias InternalSegments =
    Array ( Int, ApiData DocumentsPage )


{-| A list of segments as it is used to view the listing
-}
type alias PresentationSegments =
    List (ApiData (List DocumentResult))


{-| Return an empty page sequence
-}
init : PageSequence
init =
    PageSequence Array.empty False


{-| Construct a subsequence that comprises the given window of the listing.
-}
presentationSegmentsLimited : Int -> PageSequence -> PresentationSegments
presentationSegmentsLimited limit (PageSequence array complete) =
    foldrWithLimit
        (\lengthSoFar ( elementLength, elementApiData ) accu ->
            RemoteData.map
                (\documentsPage ->
                    documentsPage.content
                        |> (if (limit - lengthSoFar) >= elementLength then
                                identity

                            else
                                List.take (limit - lengthSoFar)
                           )
                )
                elementApiData
                :: accu
        )
        limit
        []
        array


{-| Construct a subsequence that comprises the given window of the listing.
-}
presentationSegmentsAll : PageSequence -> PresentationSegments
presentationSegmentsAll (PageSequence array complete) =
    Array.foldr
        (\( elementLength, elementApiData ) accu ->
            RemoteData.map .content elementApiData
                :: accu
        )
        []
        array


{-| Get the first document from PresentationSegments if existent and cached
-}
firstDocument : PresentationSegments -> Maybe DocumentResult
firstDocument thePresentationSegments =
    thePresentationSegments
        |> List.head
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.andThen List.head


{-| Construct a flattened version of PresentationSegments.
Segments with missing ApiData are represented by an element with value of Nothing.
-}
partialListOfDocumentResults : PresentationSegments -> List (Maybe DocumentResult)
partialListOfDocumentResults thePresentationSegments =
    thePresentationSegments
        |> List.map
            (RemoteData.unwrap
                [ Nothing ]
                (List.map Just)
            )
        |> List.concat


{-| Find a document by id along with its direct neighbours.
-}
findAdjacentDocuments : DocumentId -> PresentationSegments -> Maybe ( Maybe DocumentResult, DocumentResult, Maybe DocumentResult )
findAdjacentDocuments documentId thePresentationSegments =
    thePresentationSegments
        |> partialListOfDocumentResults
        |> Utils.List.findAdjacent
            (\documentResultOrHole ->
                case documentResultOrHole of
                    Nothing ->
                        False

                    Just documentResult ->
                        documentResult.document.id == documentId
            )
        |> Maybe.andThen
            (\( prevMaybe, thisMaybe, nextMaybe ) ->
                thisMaybe
                    |> Maybe.map
                        (\this ->
                            ( Maybe.Extra.join prevMaybe
                            , this
                            , Maybe.Extra.join nextMaybe
                            )
                        )
            )


{-| Find a document by id and return its index, i.e. the number given in the DocumentResult.

  - Returns `Success (Just index)` if the document is found.
  - Returns `Success Nothing` if the document is not found and all segments are successfully loaded.
  - Returns a non-`Success` RemoteData value otherwise.

-}
findIndex : DocumentId -> PresentationSegments -> ApiData (Maybe Int)
findIndex documentId thePresentationSegments =
    let
        list : List (ApiData (Maybe Int))
        list =
            thePresentationSegments
                |> List.map
                    (RemoteData.map
                        (\documentResults ->
                            documentResults
                                |> List.Extra.findMap
                                    (\documentResult ->
                                        if documentResult.document.id == documentId then
                                            Just documentResult.number

                                        else
                                            Nothing
                                    )
                        )
                    )

        maybeIndex : Maybe Int
        maybeIndex =
            list
                |> List.Extra.findMap
                    (RemoteData.unwrap Nothing identity)
    in
    if maybeIndex /= Nothing then
        Success maybeIndex

    else
        list
            |> RemoteData.fromList
            |> RemoteData.map (always Nothing)


{-| -}
canLoadMore : Int -> PageSequence -> Bool
canLoadMore limit (PageSequence array complete) =
    not complete || limit < numberOfRequestedResults array


{-| -}
remoteDataIsSuccess : PageSequence -> Bool
remoteDataIsSuccess (PageSequence array complete) =
    array
        |> Array.toList
        |> List.all
            (Tuple.second >> RemoteData.isSuccess)


{-| Returns the number of results so far, and an indication if there may be more.
-}
extent : PageSequence -> ( Int, Bool )
extent (PageSequence array complete) =
    let
        ( knownResultsCount, allSegmentsSuccessfullyLoaded ) =
            Array.foldl
                (\( elementLength, elementApiData ) ( sum, successful ) ->
                    elementApiData
                        |> RemoteData.unwrap
                            ( sum, False )
                            (\documentsPage ->
                                ( sum + List.length documentsPage.content
                                , successful
                                )
                            )
                )
                ( 0, True )
                array
    in
    ( knownResultsCount
    , allSegmentsSuccessfullyLoaded && complete
    )


{-| Determine if a given page sequence fulfills the needs to show a given window of a listing
-}
statusOfNeededWindow : Int -> PageSequence -> Needs.Status
statusOfNeededWindow limit (PageSequence array complete) =
    if limit > numberOfRequestedResults array && not complete then
        NotRequested

    else
        foldrWithLimit
            (\lengthSoFar ( elementLength, elementApiData ) accu ->
                Needs.statusPlus
                    (Needs.statusFromRemoteData elementApiData)
                    accu
            )
            limit
            Fulfilled
            array


{-| Possibly add a new page (with state `Loading`) to the page sequence,
so that the whole limit is covered by the sequence.

Also returns the page index and the window that needs to be queried in addition.

-}
requestWindow : Int -> PageSequence -> ( Maybe ( Int, Window ), PageSequence )
requestWindow limit ((PageSequence array complete) as pageSequence) =
    let
        currentLength =
            numberOfRequestedResults array
    in
    if complete || limit <= currentLength then
        ( Nothing
        , pageSequence
        )

    else
        ( Just
            ( Array.length array
            , { offset = currentLength, limit = limit - currentLength }
            )
        , PageSequence
            (Array.push ( limit - currentLength, Loading ) array)
            False
        )


{-| Update a page (most likely the last one) of the sequence after recieving the query's result.
-}
updatePageResult : Int -> Window -> ApiData DocumentsPage -> PageSequence -> PageSequence
updatePageResult pageIndex window apiData (PageSequence segments complete) =
    PageSequence
        (Array.set
            pageIndex
            ( window.limit, apiData )
            segments
        )
        (complete
            || RemoteData.unwrap
                False
                (\documentsPage -> not documentsPage.hasNextPage)
                apiData
        )


numberOfRequestedResults : InternalSegments -> Int
numberOfRequestedResults array =
    Array.foldl
        (\( elementLength, elementApiData ) sum ->
            elementLength + sum
        )
        0
        array


foldrWithLimit : (Int -> ( Int, ApiData DocumentsPage ) -> b -> b) -> Int -> b -> InternalSegments -> b
foldrWithLimit fn limit start array =
    let
        step : Int -> Int -> b
        step length index =
            if limit <= length then
                start

            else
                case Array.get index array of
                    Nothing ->
                        start

                    Just (( elementLength, elementApiData ) as element) ->
                        fn length element (step (length + elementLength) (index + 1))
    in
    step 0 0
