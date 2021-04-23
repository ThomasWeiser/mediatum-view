module Entities.Markup.Parser exposing
    ( Segments, Segment(..)
    , parse, parseTestable
    )

{-|

@docs Segments, Segment
@docs parse, parseTestable

-}

import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)


startTag : String
startTag =
    "<mediatum:fts>"


endTag : String
endTag =
    "</mediatum:fts>"


{-| Parsed markup is a list of segments.
-}
type alias Segments =
    List Segment


{-| A segment represents either regular text or a markup element with embedded text.
-}
type Segment
    = Text String
    | Fts String


{-| Decompose a string with markup.

    parse "foo<mediatum:fts>bar</mediatum:fts>baz"
        == [ Text "foo"
           , Fts "bar"
           , Text "baz"
           ]

-}
parse : String -> Segments
parse text =
    parseTestable text
        |> Result.withDefault [ Text text ]


{-| The Parser is written in a way that it should never result in a dead end.

For testing this property we expose the function that returns a Result nevertheless.

-}
parseTestable : String -> Result (List Parser.DeadEnd) Segments
parseTestable text =
    Parser.run
        theParser
        text
        |> Result.map postprocess


postprocess : Segments -> Segments
postprocess =
    List.filter
        (\segment -> segment /= Fts "")


theParser : Parser Segments
theParser =
    Parser.getSource
        |> Parser.andThen
            (\source ->
                Parser.loop [] (parserOuterLoop source)
            )


{-| Loop through a sequence of text and markup element fragments
-}
parserOuterLoop : String -> Segments -> Parser (Parser.Step Segments Segments)
parserOuterLoop source state =
    Parser.getOffset
        |> Parser.andThen
            (\startOffset ->
                Parser.loop
                    ()
                    (parserInnerLoop source startOffset)
                    |> Parser.map
                        (\innerLoopValue ->
                            let
                                newState =
                                    state
                                        |> Maybe.Extra.cons innerLoopValue.maybeTextFragment
                                        |> Maybe.Extra.cons innerLoopValue.maybeElementFragment
                            in
                            if innerLoopValue.maybeElementFragment == Nothing then
                                Parser.Done (List.reverse newState)

                            else
                                Parser.Loop newState
                        )
            )


type alias InnerLoopValue =
    { maybeTextFragment : Maybe Segment
    , maybeElementFragment : Maybe Segment
    }


{-| Chomp regular text, followed by either a markup element or the end of input
-}
parserInnerLoop : String -> Int -> () -> Parser (Parser.Step () InnerLoopValue)
parserInnerLoop source startOffset _ =
    let
        innerLoopValue : Int -> Maybe Segment -> InnerLoopValue
        innerLoopValue endOffset maybeElementFragment =
            { maybeTextFragment =
                if endOffset > startOffset then
                    Just (Text (String.slice startOffset endOffset source))

                else
                    Nothing
            , maybeElementFragment = maybeElementFragment
            }
    in
    Parser.oneOf
        [ Parser.succeed
            (\endOffset -> Parser.Done (innerLoopValue endOffset Nothing))
            |. Parser.end
            |= Parser.getOffset
        , Parser.succeed
            (\endOffset element -> Parser.Done (innerLoopValue endOffset (Just element)))
            |= Parser.getOffset
            |= Parser.backtrackable parserFtsElement
        , Parser.succeed (Parser.Loop ())
            |. Parser.chompIf (always True)
        ]


parserFtsElement : Parser Segment
parserFtsElement =
    Parser.succeed Fts
        |. Parser.symbol startTag
        |= Parser.getChompedString
            (Parser.chompUntil endTag)
        |. Parser.symbol endTag
