module Types.ApiData exposing
    ( ApiData
    , ApiError, apiErrorToString
    )

{-| A specialization of [`RemoteData e a`](/packages/krisajenkins/remotedata/6.0.1/RemoteData#RemoteData)
where the error type `e` is defined by `ApiError`.

Any `RemoteData` used in this module uses this error type and is therefore an `ApiData`.


# Data

@docs ApiData


# Error handling

@docs ApiError, apiErrorToString

-}

import Api
import RemoteData exposing (RemoteData)


{-| An `ApiData` is a specialization of `RemoteData` with a custom error type `Api.Error`.
-}
type alias ApiData a =
    RemoteData ApiError a


{-| The type of errors that may be reported in an `ApiData.Failure`.
It's the same as `Api.Error`.
-}
type alias ApiError =
    Api.Error


{-| Describe an `ApiError` as text (aimed for debugging)
-}
apiErrorToString : ApiError -> String
apiErrorToString apiError =
    Api.errorToString apiError
