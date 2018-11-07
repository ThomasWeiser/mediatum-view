module GenericNode exposing (GenericNode(..))

import Document exposing (Document)
import Folder exposing (Folder)


type GenericNode
    = IsFolder (List Folder)
    | IsDocument Document
    | IsNeither
