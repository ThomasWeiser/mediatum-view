module Entities.PageSequence exposing
    ( PageSequence, init, windowAsList
    , statusOfNeededWindow, requestWindow, updatePageResult
    )

{-| Listings of documents may get queryied in several consecutive pages,
e.g. by a UI button to load more results.

The type `PageSequence` represents such a sequence of pages
as it is managed by the cache.

The segmentation of the listing into pages reflects the history of requests to prolong the listing.

@docs PageSequence, init, windowAsList

@docs statusOfNeededWindow, requestWindow, updatePageResult

-}

import Entities.DocumentResults exposing (DocumentsPage)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Types exposing (Window)
import Types.ApiData exposing (ApiData)
import Types.Needs as Needs exposing (Status(..))


{-| The type `PageSequence` represents a sequence of pages that are stiched together for a listing
-}
type PageSequence
    = PageSequence Segments Bool


type alias Segments =
    List ( Int, ApiData DocumentsPage )


{-| Return an empty page sequence
-}
init : PageSequence
init =
    PageSequence [] False


{-| Construct a subsequence that comprises the given window of the listing.
-}



-- TODO


windowAsList : Int -> PageSequence -> List (ApiData DocumentsPage)
windowAsList limit (PageSequence segments complete) =
    let
        step : Int -> Segments -> List (ApiData DocumentsPage)
        step length list =
            case list of
                [] ->
                    []

                (( elementLength, elementApiData ) as element) :: tail ->
                    if limit <= length then
                        []

                    else if 0 >= length + elementLength then
                        step (length + elementLength) tail

                    else
                        RemoteData.map
                            (Types.sectionOfWindowPage
                                { offset = 0 - length
                                , limit = limit
                                }
                            )
                            elementApiData
                            :: step (length + elementLength) tail
    in
    step 0 segments


{-| Determine if a given page sequence fulfills the needs to show a given window of a listing
-}
statusOfNeededWindow : Int -> PageSequence -> Needs.Status
statusOfNeededWindow limit (PageSequence segments complete) =
    let
        step : Int -> Segments -> Needs.Status
        step length list =
            case list of
                [] ->
                    if limit > length && not complete then
                        NotRequested

                    else
                        Fulfilled

                (( elementLength, elementApiData ) as element) :: tail ->
                    Needs.statusPlus
                        (if limit > length then
                            Needs.statusFromRemoteData elementApiData

                         else
                            Fulfilled
                        )
                        (step (length + elementLength) tail)
    in
    step 0 segments


{-| Possibly add a new page (with state `Loading`) to the page sequence,
so that the whole given window is covered by the sequence.

Also returns the page index and the window that needs to be queried in addition.

-}
requestWindow : Int -> PageSequence -> ( Maybe ( Int, Window ), PageSequence )
requestWindow limit (PageSequence segments complete) =
    let
        step : Int -> Int -> Segments -> ( Maybe ( Int, Window ), Segments )
        step index length list =
            case list of
                [] ->
                    requestWithExistingLength index length

                (( elementLength, elementApiData ) as element) :: tail ->
                    if elementApiData == NotAsked then
                        requestWithExistingLength index length

                    else
                        let
                            ( tailMaybeWindow, tailPageSequence ) =
                                step (index + 1) (length + elementLength) tail
                        in
                        ( tailMaybeWindow
                        , element :: tailPageSequence
                        )

        requestWithExistingLength index length =
            if limit > length then
                ( Just ( index, { offset = length, limit = limit - length } )
                , [ ( limit - length
                    , Loading
                    )
                  ]
                )

            else
                ( Nothing
                , []
                )
    in
    if complete then
        ( Nothing, PageSequence segments True )

    else
        step 0 0 segments
            |> Tuple.mapSecond (\resultSegments -> PageSequence resultSegments False)


{-| Update a page (most likely the last one) of the sequence after recieving the query's result.
-}
updatePageResult : Int -> Window -> ApiData DocumentsPage -> PageSequence -> PageSequence
updatePageResult pageIndex window apiData (PageSequence segments complete) =
    PageSequence
        (List.Extra.setAt
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
