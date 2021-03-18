module Entities.PageSequence exposing
    ( PageSequence, init, toList
    , statusOfNeededWindow, requestWindow, updatePageResult
    )

{-| Listings of documents may get queryied in several consecutive pages,
e.g. by a UI button to load more results.

The type `PageSequence` represents such a sequence of pages
as it is managed by the cache.

The segmentation of the listing into pages reflects the history of requests to prolong the listing.

@docs PageSequence, init, toList

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
    = PageSequence PageSequenceIntern


type alias PageSequenceIntern =
    List ( Int, ApiData DocumentsPage )


{-| Return an empty page sequence
-}
init : PageSequence
init =
    PageSequence []


{-| Return the list of pages, given as `ApiData DocumentsPage`
-}
toList : PageSequence -> List (ApiData DocumentsPage)
toList (PageSequence pageSequence) =
    List.map Tuple.second pageSequence


{-| Determine if a given page sequence fullfills the needs to show a given window of a listing
-}
statusOfNeededWindow : Window -> PageSequence -> Needs.Status
statusOfNeededWindow window (PageSequence pageSequence) =
    let
        neededLength =
            window.offset + window.limit

        step : Int -> PageSequenceIntern -> Needs.Status
        step length sequence =
            case sequence of
                [] ->
                    if neededLength > length then
                        NotRequested

                    else
                        Fulfilled

                (( elementLength, elementApiData ) as element) :: tail ->
                    Needs.statusPlus
                        (if neededLength > length then
                            Needs.statusFromRemoteData elementApiData

                         else
                            Fulfilled
                        )
                        (step (length + elementLength) tail)
    in
    step 0 pageSequence


{-| Possibly add a new page (with state `Loading`) to the page sequence,
so that the whole given window is covered by the sequence.

Also returns the page index and the window that needs to be queried in addition.

-}
requestWindow : Window -> PageSequence -> ( Maybe ( Int, Window ), PageSequence )
requestWindow window (PageSequence pageSequence) =
    let
        neededLength =
            window.offset + window.limit

        step : Int -> Int -> PageSequenceIntern -> ( Maybe ( Int, Window ), PageSequenceIntern )
        step index length sequence =
            case sequence of
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
            if neededLength > length then
                ( Just ( index, { offset = length, limit = neededLength - length } )
                , [ ( neededLength - length
                    , Loading
                    )
                  ]
                )

            else
                ( Nothing
                , []
                )
    in
    step 0 0 pageSequence
        |> Tuple.mapSecond PageSequence


{-| Update a page (most likely the last one) of the sequence after recieving the query's result.
-}
updatePageResult : Int -> Window -> ApiData DocumentsPage -> PageSequence -> PageSequence
updatePageResult pageIndex window apiData (PageSequence pageSequence) =
    List.Extra.setAt
        pageIndex
        ( window.limit, apiData )
        pageSequence
        |> PageSequence
