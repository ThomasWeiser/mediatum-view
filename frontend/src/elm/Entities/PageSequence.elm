module Entities.PageSequence exposing
    ( PageSequence, init
    , PresentationSegments, presentationSegments
    , statusOfNeededWindow, requestWindow, updatePageResult
    )

{-| Listings of documents may get queryied in several consecutive pages,
e.g. by a UI button to load more results.

The type `PageSequence` represents such a sequence of pages as it is stored in the cache.

The segmentation of the listing into pages reflects the history of requests to prolong the listing.

@docs PageSequence, init
@docs PresentationSegments, presentationSegments

@docs statusOfNeededWindow, requestWindow, updatePageResult

-}

import Array exposing (Array)
import Entities.DocumentResults exposing (DocumentResult, DocumentsPage)
import RemoteData exposing (RemoteData(..))
import Types exposing (Window)
import Types.ApiData exposing (ApiData)
import Types.Needs as Needs exposing (Status(..))


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
presentationSegments : Int -> PageSequence -> PresentationSegments
presentationSegments limit (PageSequence array complete) =
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


{-| Determine if a given page sequence fulfills the needs to show a given window of a listing
-}
statusOfNeededWindow : Int -> PageSequence -> Needs.Status
statusOfNeededWindow limit (PageSequence array complete) =
    if limit > numberOfResults array && not complete then
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
            numberOfResults array
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


numberOfResults : InternalSegments -> Int
numberOfResults array =
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
