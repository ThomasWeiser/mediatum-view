module Pagination.Offset.Page exposing
    ( Page
    , PageResult
    , Position(..)
    , entries
    , initialPageResult
    , isFirstPage
    , loadingPageResult
    , positionToOffset
    , updatePageResultFromResult
    )

import Graphql.Extra


type alias Page itemModel =
    { offset : Int
    , hasNextPage : Bool
    , content : List itemModel
    }


type Position
    = First
    | Previous
    | Next


entries : Page itemModel -> List itemModel
entries page =
    page.content


isFirstPage : Page itemModel -> Bool
isFirstPage page =
    page.offset == 0


type alias PageResult itemModel =
    { loading : Bool
    , error : Maybe Graphql.Extra.StrippedError
    , page : Maybe (Page itemModel)
    }


initialPageResult : PageResult itemModel
initialPageResult =
    { loading = False
    , error = Nothing
    , page = Nothing
    }


updatePageResultFromResult :
    Result Graphql.Extra.StrippedError (Page itemModel)
    -> PageResult itemModel
    -> PageResult itemModel
updatePageResultFromResult result pageResult =
    case result of
        Err err ->
            { pageResult | loading = False, error = Just err }

        Ok content ->
            { loading = False, error = Nothing, page = Just content }


loadingPageResult : PageResult itemModel -> PageResult itemModel
loadingPageResult pageResult =
    { pageResult | loading = True }


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
                    offset
                        - pageSize
                        |> Basics.max 0

        Next ->
            case referencePage of
                Nothing ->
                    0

                Just { offset, hasNextPage } ->
                    if hasNextPage then
                        offset + pageSize

                    else
                        offset
