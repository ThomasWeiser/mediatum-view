module Types.SearchTerm exposing
    ( SearchTerm
    , fromString
    , fromStringWithDefault
    , toString
    , ordering
    , SetOfSearchTerms
    , emptySet
    , setIsEmpty
    , setFromList
    , setToList
    )

{-|

@docs SearchTerm
@docs fromString
@docs fromStringWithDefault
@docs toString
@docs ordering

@docs SetOfSearchTerms
@docs emptySet
@docs setIsEmpty
@docs setFromList
@docs setToList

-}

import List.Unique
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

Returns `Nothing` if the result would be the empty.

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


{-| A set of unique search terms
-}
type SetOfSearchTerms
    = SetOfSearchTerms (List.Unique.UniqueList SearchTerm)


{-| -}
emptySet : SetOfSearchTerms
emptySet =
    SetOfSearchTerms List.Unique.empty


{-| -}
setIsEmpty : SetOfSearchTerms -> Bool
setIsEmpty (SetOfSearchTerms set) =
    List.Unique.isEmpty set


{-| -}
setFromList : List SearchTerm -> SetOfSearchTerms
setFromList list =
    SetOfSearchTerms <|
        List.Unique.fromList list


{-| -}
setToList : SetOfSearchTerms -> List SearchTerm
setToList (SetOfSearchTerms set) =
    List.Unique.toList set
