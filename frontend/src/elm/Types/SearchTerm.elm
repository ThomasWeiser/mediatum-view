module Types.SearchTerm exposing
    ( SearchTerm
    , SetOfSearchTerms
    , emptySet
    , fromString
    , fromStringWithDefault
    , ordering
    , setFromList
    , setIsEmpty
    , setToList
    , toString
    )

import List.Unique
import Ordering exposing (..)
import String.Extra


{-| A search term is a string that is

  - non-empty
  - has no whitespace at either end
  - has no repeated whitespace within the string

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


type SetOfSearchTerms
    = SetOfSearchTerms (List.Unique.UniqueList SearchTerm)


emptySet : SetOfSearchTerms
emptySet =
    SetOfSearchTerms List.Unique.empty


setIsEmpty : SetOfSearchTerms -> Bool
setIsEmpty (SetOfSearchTerms set) =
    List.Unique.isEmpty set


setFromList : List SearchTerm -> SetOfSearchTerms
setFromList list =
    SetOfSearchTerms <|
        List.Unique.fromList list


setToList : SetOfSearchTerms -> List SearchTerm
setToList (SetOfSearchTerms set) =
    List.Unique.toList set
