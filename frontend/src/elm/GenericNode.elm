module GenericNode exposing (GenericNode(..))

import Data.Types exposing (Document, Folder)
import List.Nonempty exposing (Nonempty)


type GenericNode
    = IsFolder (Nonempty Folder)
    | IsDocument Document
    | IsNeither
