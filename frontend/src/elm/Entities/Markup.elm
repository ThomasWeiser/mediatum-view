module Entities.Markup exposing
    ( Markup
    , parse
    , empty, plainText, normalizeYear
    , isEmpty
    , trim, view
    )

{-|

@docs Markup
@docs parse
@docs empty, plainText, normalizeYear
@docs isEmpty
@docs trim, view

-}

import Entities.Markup.Parser exposing (Segment(..), Segments)
import Html exposing (Html)
import Html.Attributes
import String.Extra


{-| A text with markup. Internaly represented as a list of segments.
-}
type Markup
    = Markup Segments


{-| An empty markup text, i.e. an empty list of segments
-}
empty : Markup
empty =
    Markup []


isEmpty : Markup -> Bool
isEmpty (Markup segments) =
    segments == []


{-| Decompose a string with markup.

    parse "foo <mediatum:fts>bar</mediatum:fts> baz"
        == Markup
            [ Text "foo "
            , Fts "bar"
            , Text " baz"
            ]

-}
parse : String -> Markup
parse =
    Markup << Entities.Markup.Parser.parse


{-| The parsed text with markup removed

    plaintext (parse "foo <mediatum:fts>bar</mediatum:fts> baz")
        == "foo bar baz"

-}
plainText : Markup -> String
plainText (Markup segments) =
    segments
        |> List.map segmentText
        |> String.concat


{-| Years are sometime formatted as "2020-00-00T00:00:00".
For a nicer display we take just the first segment and only the first 4 characters of it.

    normalizeYear (parse "<mediatum:fts>2020</mediatum:fts>-00-00T00:00:00")
        == Markup [ Fts "2020" ]

-}
normalizeYear : Markup -> Markup
normalizeYear (Markup segments) =
    segments
        |> List.take 1
        |> List.map
            (mapSegment
                (String.left 4)
            )
        |> Markup


{-| Limits the length of the Markup approximately to a certain number of characters.

We don't cut within Fts segments. So the result may be a bit longer then given.

-}
trim : Int -> Markup -> Markup
trim length0 (Markup segments0) =
    let
        step length segments =
            if length <= 0 then
                []

            else
                case segments of
                    [] ->
                        []

                    segment :: tail ->
                        case segment of
                            Text string ->
                                let
                                    trunc =
                                        String.Extra.softEllipsis length string
                                in
                                if trunc == string then
                                    Text string :: step (length - String.length string) tail

                                else
                                    [ Text trunc ]

                            Fts string ->
                                Fts string :: step (length - String.length string) tail
    in
    Markup (step length0 segments0)


{-| Map markup to a Html.span element. Fts segments get marked with class "highlight".

    view (parse "foo <mediatum:fts>bar</mediatum:fts> baz")
        == Html.span []
            [ Html.text "foo "
            , Html.span
                [ Html.Attributes.class "highlight" ]
                [ Html.text "bar" ]
            , Html.text " baz"
            ]

-}
view : Markup -> Html msg
view (Markup segments) =
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
