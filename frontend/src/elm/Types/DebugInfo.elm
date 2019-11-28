module Types.DebugInfo exposing (DebugInfo, debugInfo)

{-| We want to store some values in the model only for inspection by the Elm debugger.

This module defines a wrapper type that prevents any other usage of such model fields.

@docs DebugInfo, debugInfo

-}


{-| Wrapper for "write-only" values, i.e. they can be constructed but not extracted.
-}
type DebugInfo a
    = DebugInfo a


{-| Wrap up a value
-}
debugInfo : a -> DebugInfo a
debugInfo =
    DebugInfo
