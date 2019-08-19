module Route.Url exposing
    ( parseUrl
    , toString
    )

import Data.Types exposing (FtsSorting(..), NodeId, nodeIdFromInt, nodeIdToInt)
import Dict
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Parser as ElmParser exposing ((|.), (|=))
import Route exposing (..)
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
            , Parser.map OneId (Parser.map nodeIdFromInt Parser.int) <?> parserParameters
            , Parser.map TwoIds
                (Parser.map nodeIdFromInt Parser.int
                    </> Parser.map nodeIdFromInt Parser.int
                )
                <?> parserParameters
            ]


parserParameters : QueryParser.Parser RouteParameters
parserParameters =
    QueryParser.map6 RouteParameters
        (QueryParser.string "fts-term"
            |> QueryParser.map
                (Maybe.andThen cleanSearchTerm)
        )
        (QueryParser.enum "fts-sorting"
            (Dict.fromList [ ( "by-rank", FtsByRank ), ( "by-date", FtsByDate ) ])
        )
        (QueryParser.string "filter-by-year"
            |> QueryParser.map
                (Maybe.andThen
                    (ElmParser.run elmParserYearRange
                        >> Result.toMaybe
                    )
                )
        )
        (QueryParser.custom "filter-by-title"
            (List.map cleanSearchTerm
                >> Maybe.Extra.values
                >> List.Nonempty.fromList
            )
        )
        (QueryParser.int "offset")
        (QueryParser.int "limit")


elmParserYearRange : ElmParser.Parser ( Int, Int )
elmParserYearRange =
    ElmParser.succeed Tuple.pair
        |= ElmParser.int
        |. ElmParser.symbol "-"
        |= ElmParser.int


toString : Route -> String
toString route =
    Builder.absolute
        (case route.path of
            NoId ->
                []

            OneId id ->
                [ id |> nodeIdToInt |> String.fromInt ]

            TwoIds id1 id2 ->
                [ id1 |> nodeIdToInt |> String.fromInt, id2 |> nodeIdToInt |> String.fromInt ]
        )
        (Maybe.Extra.values
            [ Maybe.map
                (Builder.string "fts-term")
                route.parameters.ftsTerm
            , Maybe.map
                (\ftsSorting ->
                    Builder.string "fts-sorting" <|
                        case ftsSorting of
                            FtsByRank ->
                                "by-rank"

                            FtsByDate ->
                                "by-date"
                )
                route.parameters.ftsSorting
            , Maybe.map
                (\( year1, year2 ) ->
                    Builder.string "filter-by-year" <|
                        String.fromInt year1
                            ++ "-"
                            ++ String.fromInt year2
                )
                route.parameters.filterByYear
            ]
            ++ Maybe.Extra.unwrap
                []
                (\byTitleList ->
                    byTitleList
                        |> List.Nonempty.toList
                        |> List.map (Builder.string "filter-by-title")
                )
                route.parameters.filterByTitle
            ++ Maybe.Extra.values
                [ Maybe.map
                    (Builder.int "offset")
                    route.parameters.offset
                , Maybe.map
                    (Builder.int "limit")
                    route.parameters.limit
                ]
        )
