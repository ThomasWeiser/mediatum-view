module Types.Needs exposing
    ( Needs
    , none, atomic, batch, sequence
    , Status(..)
    , statusFromRemoteData, statusFromListOfRemoteData, statusPlus
    , StatusOfAtomicNeed, RequestAtomicNeed, target
    , flatten
    )

{-| Generic types and functions to manage a collection of needs.

@docs Needs


# Constructing a collection of needs

The type variable `n` stands for an atomic need.

@docs none, atomic, batch, sequence


# Status of a need

@docs Status
@docs statusFromRemoteData, statusFromListOfRemoteData, statusPlus


# Targeting needs

@docs StatusOfAtomicNeed, RequestAtomicNeed, target


# Helper functions

@docs flatten

-}

import RemoteData exposing (RemoteData(..))


{-| A collection of needs.

This is either

  - an atomic need [as defined](Cache#Need) by the application,
  - or a batch of needs that should be targeted all at once,
  - or a sequence of needs with a part that should be satisfied first before the second part is targeted.

-}
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


{-| Flatten a nested tree of needs as much as possible.

Useful for better readability when displaying values during debugging.

-}
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


{-| The status of a need or a collection of needs, with regard to the cached data
-}
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


{-| Map a `RemoteData` value to a `Status` value.

Note that we don't have a mechanism for retrying failed requests.
Therefore a value of `RemoteData.Failure e` is mapped to `OnGoing`.

-}
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


{-| Map a list of `RemoteData` values to a `Status` value.

If any of the data is `NotAsked` the state is `NotRequested`.
If all data of `Success a` then the state is `Fulfilled`.
Otherwise the state is `OnGoing`.

-}
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


{-| The cache management has to provide a function to evaluate the status of an atomic need.

Note that the complete type of the provided function will probably be `Cache.Model -> Need -> Status`.

The first argument will already be supplied by the cache module when passing the function to `target`.

So the type as passed to `target` is just `n -> Status` (where `n` is the type of an atomic need).

-}
type alias StatusOfAtomicNeed n =
    n -> Status


{-| The cache management has to provide a function to request data in order to satisfy a unfulfilled need.

Note that the function should do two things:

  - Return a `Cmd` with the necessary API request(s).

  - Map a value of type `a` (which stands for the current cache content)
    in order to mark the corresponding cache entries as `RemoteData.Loading`.

-}
type alias RequestAtomicNeed n a msg =
    n -> a -> ( a, Cmd msg )


{-| Target a collection of needs:

  - Evaluate the status of each contained atomic need.
  - For each need with status `NotRequested`:
      - Produce an API request to get the corresponding data.
      - Mark the data as `Loading` in the cache.

The type variables are:

  - `n`: atomic need
  - `a`: cache model
  - `msg`: message type of the cache update function, particularly handling the results of the API requests.

-}
target : StatusOfAtomicNeed n -> RequestAtomicNeed n a msg -> Needs n -> a -> ( a, Cmd msg )
target statusOfAtomicNeed requestAtomicNeed needs acc =
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
                            target statusOfAtomicNeed requestAtomicNeed needs1 acc1
                    in
                    ( acc2, Cmd.batch [ cmd1, cmd2 ] )
                )
                ( acc, Cmd.none )
                listOfNeeds

        Sequence needsFirst needsSecond ->
            if statusOfNeeds statusOfAtomicNeed needsFirst == Fulfilled then
                target statusOfAtomicNeed requestAtomicNeed needsSecond acc

            else
                target statusOfAtomicNeed requestAtomicNeed needsFirst acc
