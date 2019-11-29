module Types.Needs exposing
    ( Needs
    , none, atomic, batch, sequence
    , Status(..)
    , statusFromRemoteData, statusFromListOfRemoteData
    , StatusOfAtomicNeed, RequestAtomicNeed, requireNeeds
    , flatten
    )

{-| Generic types and functions to manage a collection of needs.

@docs Needs
@docs none, atomic, batch, sequence
@docs Status
@docs statusFromRemoteData, statusFromListOfRemoteData
@docs StatusOfAtomicNeed, RequestAtomicNeed, requireNeeds
@docs flatten

-}

import RemoteData exposing (RemoteData(..))


{-| -}
type Needs n
    = Atomic n
    | Batch (List (Needs n))
    | Sequence (Needs n) (Needs n)


{-| -}
none : Needs n
none =
    Batch []


{-| -}
atomic : n -> Needs n
atomic =
    Atomic


{-| -}
batch : List (Needs n) -> Needs n
batch =
    Batch


{-| -}
sequence : Needs n -> Needs n -> Needs n
sequence =
    Sequence


{-| -}
flatten : Needs n -> Needs n
flatten needs =
    let
        asList : Needs n -> List (Needs n)
        asList needs1 =
            case needs1 of
                Atomic _ ->
                    [ needs1 ]

                Batch listOfNeeds ->
                    listOfNeeds

                Sequence _ _ ->
                    [ needs1 ]
    in
    case needs of
        Atomic _ ->
            needs

        Batch listOfNeeds ->
            listOfNeeds
                |> List.concatMap (flatten >> asList)
                |> Batch

        Sequence needsFirst needsSecond ->
            case flatten needsFirst of
                Batch [] ->
                    flatten needsSecond

                needsFirstFlat ->
                    case flatten needsSecond of
                        Batch [] ->
                            needsFirstFlat

                        needsSecondFlat ->
                            Sequence
                                needsFirstFlat
                                needsSecondFlat


{-| -}
type Status
    = NotRequested
    | Fulfilled
    | OnGoing


statusPlus : Status -> Status -> Status
statusPlus statusOne statusTwo =
    if statusOne == NotRequested || statusTwo == NotRequested then
        NotRequested

    else if statusOne == OnGoing || statusTwo == OnGoing then
        OnGoing

    else
        Fulfilled


{-| -}
statusFromRemoteData : RemoteData e a -> Status
statusFromRemoteData remoteData =
    case remoteData of
        NotAsked ->
            NotRequested

        Loading ->
            OnGoing

        Failure _ ->
            OnGoing

        Success _ ->
            Fulfilled


{-| -}
statusFromListOfRemoteData : List (RemoteData e a) -> Status
statusFromListOfRemoteData listOfRemoteData =
    if List.any RemoteData.isNotAsked listOfRemoteData then
        NotRequested

    else if List.all RemoteData.isSuccess listOfRemoteData then
        Fulfilled

    else
        OnGoing


statusOfNeeds : (n -> Status) -> Needs n -> Status
statusOfNeeds statusOfAtomicNeed needs =
    case needs of
        Atomic need ->
            statusOfAtomicNeed need

        Batch listOfNeeds ->
            List.foldl
                (\subNeeds statucAcc ->
                    statusPlus
                        (statusOfNeeds statusOfAtomicNeed subNeeds)
                        statucAcc
                )
                Fulfilled
                listOfNeeds

        Sequence needsFirst needsSecond ->
            let
                statusFirst =
                    statusOfNeeds statusOfAtomicNeed needsFirst
            in
            if statusFirst /= Fulfilled then
                statusFirst

            else
                statusOfNeeds statusOfAtomicNeed needsSecond


{-| -}
type alias StatusOfAtomicNeed n =
    n -> Status


{-| -}
type alias RequestAtomicNeed n a msg =
    n -> a -> ( a, Cmd msg )


{-| -}
requireNeeds : StatusOfAtomicNeed n -> RequestAtomicNeed n a msg -> Needs n -> a -> ( a, Cmd msg )
requireNeeds statusOfAtomicNeed requestAtomicNeed needs acc =
    case needs of
        Atomic need ->
            if statusOfAtomicNeed need == NotRequested then
                requestAtomicNeed need acc

            else
                ( acc, Cmd.none )

        Batch listOfNeeds ->
            List.foldl
                (\needs1 ( acc1, cmd1 ) ->
                    let
                        ( acc2, cmd2 ) =
                            requireNeeds statusOfAtomicNeed requestAtomicNeed needs1 acc1
                    in
                    ( acc2, Cmd.batch [ cmd1, cmd2 ] )
                )
                ( acc, Cmd.none )
                listOfNeeds

        Sequence needsFirst needsSecond ->
            if statusOfNeeds statusOfAtomicNeed needsFirst == Fulfilled then
                requireNeeds statusOfAtomicNeed requestAtomicNeed needsSecond acc

            else
                requireNeeds statusOfAtomicNeed requestAtomicNeed needsFirst acc
