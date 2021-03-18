module Entities.PageSequence exposing (PageSequence, requestWindow, statusOfNeededWindow, updatePage)

import Entities.DocumentResults exposing (DocumentsPage)
import RemoteData exposing (RemoteData(..))
import Types exposing (Window)
import Types.ApiData exposing (ApiData)
import Types.Needs as Needs exposing (Status(..))


{-| Listings of documents may get queryied in several consecutive pages,
e.g. by a UI button to load more results.

The type `PageSequence` represents such a sequence of pages
as it is managed by the cache.

@docs PageSequence
@docs statusOfNeededWindow, requestWindow, updatePage

-}
type alias PageSequence =
    List ( Int, ApiData DocumentsPage )


statusOfNeededWindow_1 : Window -> PageSequence -> Needs.Status
statusOfNeededWindow_1 window pageSequence =
    let
        neededLength =
            window.offset + window.limit
    in
    pageSequence
        |> List.foldl
            (\( elementLength, elementApiData ) ( accuLength, accuStatus ) ->
                ( accuLength + elementLength
                , if neededLength < accuLength then
                    accuStatus

                  else
                    Needs.statusPlus
                        accuStatus
                        (Needs.statusFromRemoteData elementApiData)
                )
            )
            ( 0, Needs.Fulfilled )
        |> Tuple.second
        |> (\res ->
                let
                    _ =
                        Debug.log "statusOfNeededWindow"
                            { window = window, pageSequence = pageSequence, res = res }
                in
                res
           )


statusOfNeededWindow : Window -> PageSequence -> Needs.Status
statusOfNeededWindow window pageSequence =
    let
        neededLength =
            window.offset + window.limit

        step : Int -> PageSequence -> Needs.Status
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

        requestWithExistingLength length =
            if neededLength > length then
                ( Just { offset = length, limit = neededLength - length }
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
    step 0 pageSequence
        |> (\res ->
                let
                    _ =
                        Debug.log "statusOfNeededWindow"
                            { window = window, pageSequence = pageSequence, res = res }
                in
                res
           )


requestWindow : Window -> PageSequence -> ( Maybe Window, PageSequence )
requestWindow window pageSequence =
    let
        neededLength =
            window.offset + window.limit

        step : Int -> PageSequence -> ( Maybe Window, PageSequence )
        step length sequence =
            case sequence of
                [] ->
                    requestWithExistingLength length

                (( elementLength, elementApiData ) as element) :: tail ->
                    if elementApiData == NotAsked then
                        requestWithExistingLength length

                    else
                        let
                            ( tailMaybeWindow, tailPageSequence ) =
                                step (length + elementLength) tail
                        in
                        ( tailMaybeWindow
                        , element :: tailPageSequence
                        )

        requestWithExistingLength length =
            if neededLength > length then
                ( Just { offset = length, limit = neededLength - length }
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
    step 0 pageSequence


updatePage : Window -> ApiData DocumentsPage -> PageSequence -> PageSequence
updatePage window apiData pageSequence =
    let
        step : Int -> PageSequence -> PageSequence
        step length sequence =
            if length == window.offset then
                [ ( window.limit, apiData ) ]

            else if length < window.offset then
                case sequence of
                    [] ->
                        -- Should never happen
                        [ ( window.offset - length, NotAsked )
                        , ( window.limit, apiData )
                        ]

                    (( elementLength, _ ) as element) :: tail ->
                        element :: step (length + elementLength) tail

            else
                -- Should never happen
                pageSequence
    in
    step 0 pageSequence
        |> (\res ->
                let
                    _ =
                        Debug.log "updatePage"
                            { window = window, apiData = apiData, pageSequence = pageSequence, res = res }
                in
                res
           )
