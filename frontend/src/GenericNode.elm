module GenericNode exposing (GenericNode(..))

import Document exposing (Document)
import Folder exposing (Folder)
import List.Nonempty exposing (Nonempty)


type GenericNode
    = IsFolder (Nonempty Folder)
    | IsDocument Document
    | IsNeither
