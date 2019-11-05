module Data.Types.SearchTerm exposing
    ( SearchTerm
    , SetOfSearchTerms
    , emptySet
    , fromString
    , fromStringWithDefault
    , ordering
    , setFromList
    , setInsert
    , setIsEmpty
    , setToList
    , toString
    )

import Ordering exposing (..)
import Sort
import Sort.Set
import String.Extra


{-| A search term is a string that is

  - non-empty
  - has no whitespace at either end
  - has no repeated whitespace within

-}
type SearchTerm
    = SearchTerm String


fromString : String -> Maybe SearchTerm
fromString =
    String.Extra.clean
        >> String.Extra.nonEmpty
        >> Maybe.map SearchTerm


fromStringWithDefault : String -> String -> SearchTerm
fromStringWithDefault default string =
    fromString string
        |> Maybe.withDefault (SearchTerm default)


toString : SearchTerm -> String
toString (SearchTerm s) =
    s


ordering : Ordering SearchTerm
ordering =
    Ordering.byField toString


type alias SetOfSearchTerms =
    Sort.Set.Set SearchTerm


emptySet : SetOfSearchTerms
emptySet =
    Sort.Set.empty
        (Sort.custom ordering)


setFromList : List SearchTerm -> SetOfSearchTerms
setFromList =
    Sort.Set.fromList
        (Sort.custom ordering)


setToList : SetOfSearchTerms -> List SearchTerm
setToList =
    Sort.Set.toList


setInsert : SearchTerm -> SetOfSearchTerms -> SetOfSearchTerms
setInsert =
    Sort.Set.insert


setIsEmpty : SetOfSearchTerms -> Bool
setIsEmpty =
    Sort.Set.isEmpty
