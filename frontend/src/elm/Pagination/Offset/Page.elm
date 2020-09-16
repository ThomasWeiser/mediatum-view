module Pagination.Offset.Page exposing
    ( Page
    , entries
    , isFirstPage
    , PageResult
    , initialPageResult
    , updatePageResultFromResult
    , loadingPageResult
    , Position(..)
    , positionToOffset
    )

{-|


#

@docs Page
@docs entries
@docs isFirstPage


#

@docs PageResult
@docs initialPageResult
@docs updatePageResultFromResult
@docs loadingPageResult


#

@docs Position
@docs positionToOffset

-}

import Api
import Basics.Extra


{-| -}
type alias Page itemModel =
    { offset : Int
    , hasNextPage : Bool
    , content : List itemModel
    }


{-| -}
entries : Page itemModel -> List itemModel
entries page =
    page.content


{-| -}
isFirstPage : Page itemModel -> Bool
isFirstPage page =
    page.offset == 0


{-| -}
type alias PageResult itemModel =
    { loading : Bool
    , error : Maybe Api.Error
    , page : Maybe (Page itemModel)
    }


{-| -}
initialPageResult : PageResult itemModel
initialPageResult =
    { loading = False
    , error = Nothing
    , page = Nothing
    }


{-| -}
updatePageResultFromResult :
    Result Api.Error (Page itemModel)
    -> PageResult itemModel
    -> PageResult itemModel
updatePageResultFromResult result pageResult =
    case result of
        Err err ->
            { pageResult | loading = False, error = Just err }

        Ok content ->
            { loading = False, error = Nothing, page = Just content }


{-| -}
loadingPageResult : PageResult itemModel -> PageResult itemModel
loadingPageResult pageResult =
    { pageResult | loading = True }


{-| -}
type Position
    = First
    | Previous
    | Next


{-| -}
positionToOffset :
    Int
    -> Maybe (Page nodeType)
    -> Position
    -> Int
positionToOffset pageSize referencePage position =
    case position of
        First ->
            0

        Previous ->
            case referencePage of
                Nothing ->
                    0

                Just { offset } ->
                    (offset - pageSize)
                        |> Basics.Extra.atLeast 0

        Next ->
            case referencePage of
                Nothing ->
                    0

                Just { offset, hasNextPage } ->
                    if hasNextPage then
                        offset + pageSize

                    else
                        offset
