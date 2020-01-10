module Types.SearchTerm exposing
    ( SearchTerm
    , fromString
    , fromStringWithDefault
    , toString
    , ordering
    )

{-|


# Search term

@docs SearchTerm
@docs fromString
@docs fromStringWithDefault
@docs toString
@docs ordering

-}

import Ordering exposing (Ordering)
import String.Extra


{-| A search term is a string with some guarantees:

  - is non-empty
  - has no whitespace at either end
  - has no repeated whitespace within the string

-}
type SearchTerm
    = SearchTerm String


{-| Construct a `SearchTerm` by removing unnecessary whitespace from the given string.

Returns `Nothing` if the result would be empty.

-}
fromString : String -> Maybe SearchTerm
fromString string =
    string
        |> String.Extra.clean
        |> String.Extra.nonEmpty
        |> Maybe.map SearchTerm


{-| Construction function that cannot fail. Used for hard-coded test cases.
-}
fromStringWithDefault : String -> String -> SearchTerm
fromStringWithDefault default string =
    fromString string
        |> Maybe.withDefault (SearchTerm default)


{-| -}
toString : SearchTerm -> String
toString (SearchTerm string) =
    string


{-| Define an ordering on the type so we can use it as a key in a `Sort.Dict`.
-}
ordering : Ordering SearchTerm
ordering =
    Ordering.byField toString
