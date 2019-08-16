module Route exposing (Route, RouteFtsSorting(..), RouteParameters, RoutePath(..), parseUrl, toString)

import Browser.Navigation
import Dict
import List.Nonempty exposing (Nonempty)
import Maybe.Extra
import Parser as ElmParser exposing ((|.), (|=))
import String.Extra
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as QueryParser


type alias Route =
    { path : RoutePath
    , parameters : RouteParameters
    }


type RoutePath
    = NoId
    | OneId Int
    | TwoIds Int Int


type alias RouteParameters =
    { ftsTerm : Maybe String
    , ftsSorting : Maybe RouteFtsSorting
    , filterByYear : Maybe ( Int, Int )
    , filterByTitle : Maybe (Nonempty String)
    , offset : Maybe Int
    , limit : Maybe Int
    }


type RouteFtsSorting
    = ByRank
    | ByDate


emptyParameters : RouteParameters
emptyParameters =
    { ftsTerm = Nothing
    , ftsSorting = Nothing
    , filterByYear = Nothing
    , filterByTitle = Nothing
    , offset = Nothing
    , limit = Nothing
    }


parser : Parser (Route -> a) a
parser =
    Parser.map Route <|
        Parser.oneOf
            [ Parser.map NoId Parser.top <?> parserParameters
            , Parser.map OneId Parser.int <?> parserParameters
            , Parser.map TwoIds (Parser.int </> Parser.int) <?> parserParameters
            ]


parserParameters : QueryParser.Parser RouteParameters
parserParameters =
    QueryParser.map6 RouteParameters
        (QueryParser.string "fts-term"
            |> QueryParser.map
                (Maybe.andThen cleanSearchTerm)
        )
        (QueryParser.enum "fts-sorting"
            (Dict.fromList [ ( "by-rank", ByRank ), ( "by-date", ByDate ) ])
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


parseUrl : Url -> Maybe Route
parseUrl url =
    Parser.parse parser url


toString : Route -> String
toString route =
    Builder.absolute
        (case route.path of
            NoId ->
                []

            OneId id ->
                [ String.fromInt id ]

            TwoIds id1 id2 ->
                [ String.fromInt id1, String.fromInt id2 ]
        )
        (Maybe.Extra.values
            [ Maybe.map
                (Builder.string "fts-term")
                route.parameters.ftsTerm
            , Maybe.map
                (\ftsSorting ->
                    Builder.string "fts-sorting" <|
                        case ftsSorting of
                            ByRank ->
                                "by-rank"

                            ByDate ->
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


cleanSearchTerm : String -> Maybe String
cleanSearchTerm =
    -- Trim the whitespace of both sides of the string
    -- and compress repeated whitespace internally to a single whitespace char.
    String.Extra.clean >> String.Extra.nonBlank
