module Types.SidebarElement exposing (SidebarElement(..))

{-|

@docs AdjustmentToSetup

-}

import Types.Aspect exposing (Aspect)


{-| -}
type SidebarElement
    = Tree
    | Facet Aspect
