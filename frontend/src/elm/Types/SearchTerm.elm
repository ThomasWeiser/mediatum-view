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


{-| A search term is a string that is

  - non-empty
  - has no whitespace at either end
  - has no repeated whitespace within the string

-}
type SearchTerm
    = SearchTerm String


{-| -}
fromString : String -> Maybe SearchTerm
fromString string =
    string
        |> String.Extra.clean
        |> String.Extra.nonEmpty
        |> Maybe.map SearchTerm


{-| -}
fromStringWithDefault : String -> String -> SearchTerm
fromStringWithDefault default string =
    fromString string
        |> Maybe.withDefault (SearchTerm default)


{-| -}
toString : SearchTerm -> String
toString (SearchTerm string) =
    string


{-| -}
ordering : Ordering SearchTerm
ordering =
    Ordering.byField toString


{-| -}
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
