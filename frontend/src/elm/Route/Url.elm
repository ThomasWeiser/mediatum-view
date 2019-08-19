module Route.Url exposing
    ( parseUrl
    , toString
    )

import Data.Types exposing (FtsSorting(..), NodeId, nodeIdFromInt, nodeIdToInt)
import Dict
import Maybe.Extra
import Parser as ElmParser exposing ((|.), (|=))
import Route exposing (..)
import Set
import String.Extra
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
            |> queryParserWithDefault ""
            |> QueryParser.map cleanSearchTerm
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
                    )
                )
        )
        (QueryParser.custom "filter-by-title"
            (List.map (cleanSearchTerm >> String.Extra.nonEmpty)
                >> Maybe.Extra.values
                >> Set.fromList
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
            [ buildParameterIfNotDefault
                (Builder.string "fts-term")
                ""
                route.parameters.ftsTerm
            , buildParameterIfNotDefault
                (ftsSortingTostring >> Builder.string "fts-sorting")
                defaultFtsSorting
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
            ++ (route.parameters.filterByTitle
                    |> Set.toList
                    |> List.map (Builder.string "filter-by-title")
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
