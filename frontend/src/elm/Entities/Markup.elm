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
    -- TODO
    Markup
        [ Html.Parser.Text
            (plainText markup |> String.left 4)
        ]


{-| Limits the length of the Markup approximately to a certain number of characters.
-}
trim : Int -> Markup -> Markup
trim lengthLimit (Markup topNodes) =
    let
        _ =
            Debug.log "trim" (Markup topNodes)

        stepNode : Int -> Node -> ( Int, Maybe Node )
        stepNode residual node =
            if residual <= 0 then
                ( residual, Nothing )

            else
                case node of
                    Html.Parser.Text text ->
                        let
                            -- TODO: This can degrade bad if residual is small and text is long
                            trunc =
                                String.Extra.softBreak residual text
                                    |> List.head
                                    |> Maybe.withDefault ""
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
                |> List.Extra.mapAccuml
                    (\residual1 subNode ->
                        let
                            ( r2, n2 ) =
                                stepNode residual subNode
                        in
                        ( residual1 - r2, n2 )
                    )
                    residual
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
        |> Debug.log "tri="


{-| -}
view : Markup -> List (Html msg)
view (Markup nodes) =
    -- TODO: Possibly use a custom implementaton of Html.Parser.Util.toVirtualDom:
    --        - To efficiently add nodes to the end of the list
    --        - To strip out comment nodes
    Html.Parser.Util.toVirtualDom nodes
