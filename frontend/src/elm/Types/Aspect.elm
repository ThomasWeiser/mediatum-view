module Types.Aspect exposing
    ( Aspect
    , fromString
    , toString
    , ordering
    )

{-|


# Aspect

@docs Aspect
@docs fromString
@docs toString
@docs ordering

-}

import Ordering exposing (Ordering)


{-| An aspect is defined by its name.

Each aspect refers to one or more (possibly preprcessed) attributes
of the documents that are used for filtering or for faceted search.
This reference is defined by the server implementation.

`Aspect` is simply a wrapper type over a string.

-}
type Aspect
    = Aspect String


{-| Construct an `Apect`
-}
fromString : String -> Aspect
fromString string =
    Aspect string


{-| -}
toString : Aspect -> String
toString (Aspect string) =
    string


{-| Define an ordering on the type so we can use it as a key in a `Sort.Dict`.
-}
ordering : Ordering Aspect
ordering =
    Ordering.byField toString
