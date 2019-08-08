module Route exposing (Route, RouteFtsSorting(..), RouteParameters, RoutePath(..), parseUrl, toString)

import Browser.Navigation
import Dict
import Maybe.Extra
import Parser as ElmParser exposing ((|.), (|=))
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
    }


type RouteFtsSorting
    = ByRank
    | ByDate


emptyParameters : RouteParameters
emptyParameters =
    { ftsTerm = Nothing
    , ftsSorting = Nothing
    , filterByYear = Nothing
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
    QueryParser.map3 RouteParameters
        (QueryParser.string "fts-term")
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
        )
