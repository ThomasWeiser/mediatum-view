module Entities.Markup exposing
    ( Markup
    , FlagUnparsable(..)
    , parse
    , empty, plainText, normalizeYear
    , isEmpty
    , trim, view
    , toHtmlString
    )

{-|

@docs Markup
@docs FlagUnparsable
@docs parse
@docs empty, plainText, normalizeYear
@docs isEmpty
@docs trim, view
@docs toHtmlString

-}

import Html exposing (Html)
import Html.Parser exposing (Node)
import Html.Parser.Util
import List.Extra
import Maybe.Extra
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

Note: On a parser error we currently fall-back to the whole input string.
We should probably have some better solution here.

-}
parse : FlagUnparsable -> String -> Markup
parse flagUnparsable inputString =
    inputString
        |> Html.Parser.run
        |> Result.withDefault
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

    plaintext (parse "foo <span>bar</span> baz")
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

    normalizeYear (parse "<span>2020</span>-00-00T00:00:00")
        == parse "2020"

Note: Currently we don't preserve the markup structure. This should get implemented someday!

-}
normalizeYear : Markup -> Markup
normalizeYear markup =
    Markup
        [ Html.Parser.Text
            (plainText markup |> String.left 4)
        ]


{-| Limits the length of the Markup approximately to a certain number of characters.

Care is taken not to truncate words, unless they are longer than 2\*lengthLimit.

-}
trim : Int -> Markup -> Markup
trim lengthLimit (Markup topNodes) =
    let
        hardLengthLimit : Int
        hardLengthLimit =
            lengthLimit * 2

        stepNode : Int -> Node -> ( Int, Maybe Node )
        stepNode residual node =
            if residual <= 0 then
                ( residual, Nothing )

            else
                case node of
                    Html.Parser.Text text ->
                        let
                            trunc =
                                String.Extra.softBreak residual text
                                    |> List.head
                                    |> Maybe.withDefault ""
                                    |> String.left hardLengthLimit
                                    |> workaroundSoftBreakIssue text
                        in
                        ( residual - String.length text
                        , Just (Html.Parser.Text trunc)
                        )

                    Html.Parser.Element tag attributes subNodes ->
                        stepNodes residual subNodes
                            |> Tuple.mapSecond
                                (Html.Parser.Element tag attributes
                                    >> Just
                                )

                    Html.Parser.Comment comment ->
                        ( residual, Just node )

        stepNodes : Int -> List Node -> ( Int, List Node )
        stepNodes residual nodes =
            nodes
                |> List.Extra.mapAccuml stepNode residual
                |> Tuple.mapSecond Maybe.Extra.values

        ( resultingResidual, resultingNodes ) =
            stepNodes lengthLimit topNodes
    in
    (if resultingResidual >= 0 then
        resultingNodes

     else
        [ Html.Parser.Element "span"
            [ ( "class", "with-ellipisis" ) ]
            resultingNodes
        , Html.Parser.Text " ..."
        ]
    )
        |> Markup


workaroundSoftBreakIssue : String -> String -> String
workaroundSoftBreakIssue org trunc =
    let
        startsWithWhitespace s =
            let
                left1 =
                    String.left 1 s
            in
            left1 /= "" && String.Extra.isBlank left1
    in
    if startsWithWhitespace org && not (startsWithWhitespace trunc) then
        " " ++ trunc

    else
        trunc


{-| Convert Markup to Elm's Html nodes
-}
view : Markup -> List (Html msg)
view (Markup nodes) =
    Html.Parser.Util.toVirtualDom nodes


{-| Turn a Markup back into a list of HTML strings. Used for testing purposes only.
-}
toHtmlString : Markup -> String
toHtmlString (Markup nodes) =
    List.map
        Html.Parser.nodeToString
        nodes
        |> String.concat
