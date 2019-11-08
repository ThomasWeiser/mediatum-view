module Route.Url exposing
    ( parseUrl
    , toString
    )

import Dict
import Maybe.Extra
import Parser as ElmParser exposing ((|.), (|=))
import Range
import Route exposing (..)
import Types exposing (FtsSorting(..))
import Types.NodeId as NodeId
import Types.SearchTerm
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as QueryParser


parseUrl : Url -> Maybe Route
parseUrl url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    Parser.map Route <|
        Parser.oneOf
            [ Parser.map NoId Parser.top <?> parserParameters
            , Parser.map OneId (Parser.map NodeId.fromInt Parser.int) <?> parserParameters
            , Parser.map TwoIds
                (Parser.map NodeId.fromInt Parser.int
                    </> Parser.map NodeId.fromInt Parser.int
                )
                <?> parserParameters
            ]


parserParameters : QueryParser.Parser RouteParameters
parserParameters =
    QueryParser.map6 RouteParameters
        (QueryParser.string "fts-term"
            |> QueryParser.map
                (Maybe.andThen Types.SearchTerm.fromString)
        )
        (QueryParser.enum "fts-sorting"
            (Dict.fromList [ ( "by-rank", FtsByRank ), ( "by-date", FtsByDate ) ])
            |> queryParserWithDefault defaultFtsSorting
        )
        (QueryParser.string "filter-by-year"
            |> QueryParser.map
                (Maybe.andThen
                    (ElmParser.run elmParserYearRange
                        >> Result.toMaybe
                        >> Maybe.map Range.fromMaybe
                        >> Maybe.Extra.join
                    )
                )
        )
        (QueryParser.custom "filter-by-title"
            (List.map Types.SearchTerm.fromString
                >> Maybe.Extra.values
                >> Types.SearchTerm.setFromList
            )
        )
        (QueryParser.int "offset"
            |> queryParserWithDefault 0
        )
        (QueryParser.int "limit"
            |> queryParserWithDefault defaultLimit
        )


queryParserWithDefault : a -> QueryParser.Parser (Maybe a) -> QueryParser.Parser a
queryParserWithDefault defaultValue parserOfMaybe =
    QueryParser.map
        (Maybe.withDefault defaultValue)
        parserOfMaybe


elmParserYearRange : ElmParser.Parser ( Maybe Int, Maybe Int )
elmParserYearRange =
    ElmParser.succeed Tuple.pair
        |= ElmParser.oneOf
            [ ElmParser.succeed Just
                |= ElmParser.int
            , ElmParser.succeed Nothing
            ]
        |. ElmParser.symbol "-"
        |= ElmParser.oneOf
            [ ElmParser.succeed Just
                |= ElmParser.int
            , ElmParser.succeed Nothing
            ]
        |. ElmParser.end


toString : Route -> String
toString route =
    Builder.absolute
        (case route.path of
            NoId ->
                []

            OneId id ->
                [ id |> NodeId.toInt |> String.fromInt ]

            TwoIds id1 id2 ->
                [ id1 |> NodeId.toInt |> String.fromInt, id2 |> NodeId.toInt |> String.fromInt ]
        )
        (Maybe.Extra.values
            [ route.parameters.ftsTerm
                |> Maybe.map
                    (Types.SearchTerm.toString
                        >> Builder.string "fts-term"
                    )
            , buildParameterIfNotDefault
                (ftsSortingTostring >> Builder.string "fts-sorting")
                defaultFtsSorting
                route.parameters.ftsSorting
            , Maybe.map
                (\range ->
                    let
                        ( maybeYear1, maybeYear2 ) =
                            Range.toMaybe range
                    in
                    Builder.string "filter-by-year" <|
                        Maybe.Extra.unwrap "" String.fromInt maybeYear1
                            ++ "-"
                            ++ Maybe.Extra.unwrap "" String.fromInt maybeYear2
                )
                route.parameters.filterByYear
            ]
            ++ (route.parameters.filterByTitle
                    |> Types.SearchTerm.setToList
                    |> List.map
                        (Types.SearchTerm.toString
                            >> Builder.string "filter-by-title"
                        )
               )
            ++ Maybe.Extra.values
                [ buildParameterIfNotDefault
                    (Builder.int "offset")
                    0
                    route.parameters.offset
                , buildParameterIfNotDefault
                    (Builder.int "limit")
                    defaultLimit
                    route.parameters.limit
                ]
        )


ftsSortingTostring : FtsSorting -> String
ftsSortingTostring ftsSorting =
    case ftsSorting of
        FtsByRank ->
            "by-rank"

        FtsByDate ->
            "by-date"


buildParameterIfNotDefault : (a -> Builder.QueryParameter) -> a -> a -> Maybe Builder.QueryParameter
buildParameterIfNotDefault mapParameter defaultValue actualValue =
    if defaultValue == actualValue then
        Nothing

    else
        Just (mapParameter actualValue)
