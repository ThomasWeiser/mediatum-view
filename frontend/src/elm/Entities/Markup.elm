module Entities.Markup exposing
    ( Markup, Segment(..)
    , parse, parseTestable
    , empty, plainText, normalizeYear
    , view
    )

{-|

@docs Markup, Segment
@docs parse, parseTestable
@docs empty, plainText, normalizeYear
@docs view

-}

import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Parser exposing (..)


startTag : String
startTag =
    "<mediatum:fts>"


endTag : String
endTag =
    "</mediatum:fts>"


{-| A text with markup. Consists of a list of segments.
-}
type alias Markup =
    List Segment


{-| A segment represents either regular text or a markup element with embedded text.
-}
type Segment
    = Text String
    | Fts String


{-| An empty list of segments
-}
empty : Markup
empty =
    []


{-| Decompose a string with markup.

    parse "foo<mediatum:fts>bar</mediatum:fts>baz"
        == [ Text "foo"
           , Fts "bar"
           , Text "baz"
           ]

-}
parse : String -> Markup
parse text =
    parseTestable text
        |> Result.withDefault [ Text text ]


{-| The parsed text with markup removed
-}
plainText : Markup -> String
plainText segments =
    segments
        |> List.map segmentText
        |> String.concat


{-| Years are sometime formatted as "2020-00-00T00:00:00".
For a nicer display we take just the first segment and only the first 4 characters of it.
-}
normalizeYear : Markup -> Markup
normalizeYear segments =
    segments
        |> List.take 1
        |> List.map
            (mapSegment
                (String.left 4)
            )


{-| Map markup to a Html.span element. Fts segments get marked with class "highlight".
-}
view : Markup -> Html msg
view segments =
    segments
        |> List.map
            (\segment ->
                case segment of
                    Text t ->
                        Html.text t

                    Fts t ->
                        Html.span
                            [ Html.Attributes.class "highlight" ]
                            [ Html.text t ]
            )
        |> Html.span []


mapSegment : (String -> String) -> Segment -> Segment
mapSegment mapping segment =
    case segment of
        Text s ->
            Text (mapping s)

        Fts s ->
            Fts (mapping s)


segmentText : Segment -> String
segmentText segment =
    case segment of
        Text s ->
            s

        Fts s ->
            s


{-| The Parser is written in a way that it should never result in a dead end.

For testing this property we expose the function that returns a Result nevertheless.

-}
parseTestable : String -> Result (List DeadEnd) Markup
parseTestable text =
    Parser.run
        theParser
        text
        |> Result.map postprocess


postprocess : Markup -> Markup
postprocess =
    List.filter
        (\segment -> segment /= Fts "")


theParser : Parser Markup
theParser =
    getSource
        |> andThen
            (\source ->
                loop [] (parserOuterLoop source)
            )


{-| Loop through a sequence of text and markup element fragments
-}
parserOuterLoop : String -> Markup -> Parser (Step Markup Markup)
parserOuterLoop source state =
    getOffset
        |> andThen
            (\startOffset ->
                loop
                    ()
                    (parserInnerLoop source startOffset)
                    |> map
                        (\innerLoopValue ->
                            let
                                newState =
                                    state
                                        |> Maybe.Extra.cons innerLoopValue.maybeTextFragment
                                        |> Maybe.Extra.cons innerLoopValue.maybeElementFragment
                            in
                            if innerLoopValue.maybeElementFragment == Nothing then
                                Done (List.reverse newState)

                            else
                                Loop newState
                        )
            )


type alias InnerLoopValue =
    { maybeTextFragment : Maybe Segment
    , maybeElementFragment : Maybe Segment
    }


{-| Chomp regular text, followed by either a markup element or the end of input
-}
parserInnerLoop : String -> Int -> () -> Parser (Step () InnerLoopValue)
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
    oneOf
        [ succeed
            (\endOffset -> Done (innerLoopValue endOffset Nothing))
            |. end
            |= getOffset
        , succeed
            (\endOffset element -> Done (innerLoopValue endOffset (Just element)))
            |= getOffset
            |= backtrackable parserFtsElement
        , succeed (Loop ())
            |. chompIf (always True)
        ]


parserFtsElement : Parser Segment
parserFtsElement =
    succeed Fts
        |. symbol startTag
        |= getChompedString
            (chompUntil endTag)
        |. symbol endTag