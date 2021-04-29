module Entities.Markup exposing
    ( Markup
    , FlagUnparsable(..)
    , parse
    , empty, plainText, normalizeYear
    , isEmpty
    , trim, view
    )

{-|

@docs Markup
@docs FlagUnparsable
@docs parse
@docs empty, plainText, normalizeYear
@docs isEmpty
@docs trim, view

-}

import Html exposing (Html)
import Html.Attributes
import Html.Parser exposing (Node)
import Html.Parser.Util
import String.Extra


{-| A text with markup. Internaly represented as a list of segments.
-}
type Markup
    = Markup (List Node)


type FlagUnparsable
    = None
    | DivClass String
    | SpanClass String


{-| An empty markup text, i.e. an empty list of segments
-}
empty : Markup
empty =
    Markup []


{-| Determine if the Markup contains no text
-}
isEmpty : Markup -> Bool
isEmpty (Markup nodes) =
    nodes == []


{-| Parse a string with markup.
-}
parse : FlagUnparsable -> String -> Markup
parse flagUnparsable inputString =
    inputString
        |> Html.Parser.run
        |> Result.withDefault
            -- TODO: Currently, on a parser error we fall-back to the whole input string-
            --       We should probably have some better solution here.
            [ case flagUnparsable of
                None ->
                    Html.Parser.Text inputString

                DivClass class ->
                    Html.Parser.Element
                        "div"
                        [ ( "class", class ) ]
                        [ Html.Parser.Text inputString ]

                SpanClass class ->
                    Html.Parser.Element
                        "span"
                        [ ( "class", class ) ]
                        [ Html.Parser.Text inputString ]
            ]
        |> Markup


{-| The parsed text with markup removed

    plaintext (parse "foo <mediatum:fts>bar</mediatum:fts> baz")
        == "foo bar baz"

-}
plainText : Markup -> String
plainText (Markup topNodes) =
    let
        plainTextFromNodes nodes =
            List.map
                (\node ->
                    case node of
                        Html.Parser.Text text ->
                            text

                        Html.Parser.Element tag attributes subNodes ->
                            plainTextFromNodes subNodes

                        Html.Parser.Comment comment ->
                            ""
                )
                nodes
                |> String.concat
    in
    plainTextFromNodes topNodes


{-| Years are sometime formatted as "2020-00-00T00:00:00".
For a nicer display we take just the first segment and only the first 4 characters of it.

    normalizeYear (parse "<mediatum:fts>2020</mediatum:fts>-00-00T00:00:00")
        == Markup [ Fts "2020" ]

-}
normalizeYear : Markup -> Markup
normalizeYear (Markup nodes) =
    -- TODO
    Markup nodes


{-| Limits the length of the Markup approximately to a certain number of characters.

We don't cut within Fts segments. So the result may be a bit longer then given.

-}
trim : Int -> Markup -> Markup
trim length0 (Markup nodes) =
    -- TODO
    Markup nodes


{-| -}
view : Markup -> List (Html msg)
view (Markup nodes) =
    -- TODO: Possibly use a custom implementaton of Html.Parser.Util.toVirtualDom:
    --        - To efficiently add nodes to the end of the list
    --        - To strip out comment nodes
    Html.Parser.Util.toVirtualDom nodes
