module Types.Route.Url exposing
    ( parseUrl
    , toString
    )

{-|

@docs parseUrl
@docs toString

-}

import Dict
import Maybe.Extra
import Parser as ElmParser exposing ((|.), (|=))
import Set
import Sort.Dict
import Types.Aspect as Aspect
import Types.Id as Id exposing (NodeId)
import Types.Range as Range
import Types.Route as Route exposing (Route, RouteParameters, RoutePath(..))
import Types.SearchTerm
import Types.Selection as Selection exposing (FtsSorting(..))
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as QueryParser
import Utils


{-| -}
parseUrl : Url -> Maybe Route
parseUrl url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    Parser.map Route <|
        Parser.oneOf
            [ Parser.map NoId Parser.top <?> parserParameters
            , Parser.map OneId parseNodeId <?> parserParameters
            , Parser.map TwoIds
                (parseNodeId </> parseNodeId)
                <?> parserParameters
            ]


parseNodeId : Parser (NodeId -> a) a
parseNodeId =
    Parser.custom "NODE_ID" <|
        String.toInt
            >> Maybe.map Id.fromInt
            >> Maybe.andThen
                (Utils.ensure Id.isValidId)


parserParameters : QueryParser.Parser RouteParameters
parserParameters =
    QueryParser.map6 RouteParameters
        (QueryParser.string "fts-term"
            |> QueryParser.map
                (Maybe.andThen Types.SearchTerm.fromString)
        )
        (QueryParser.enum "fts-sorting"
            (Dict.fromList [ ( "by-rank", FtsByRank ), ( "by-date", FtsByDate ) ])
            |> queryParserWithDefault Route.defaultFtsSorting
        )
        (QueryParser.custom "filter-by-fts"
            (List.map
                (ElmParser.run elmParserFilter
                    >> Result.toMaybe
                )
                >> List.map
                    (Maybe.andThen
                        (\( aspectString, searchTermString ) ->
                            Types.SearchTerm.fromString searchTermString
                                |> Maybe.map
                                    (\searchTerm ->
                                        ( Aspect.fromString aspectString
                                        , searchTerm
                                        )
                                    )
                        )
                    )
                >> Maybe.Extra.values
                >> Selection.ftsFiltersFromList
            )
        )
        (QueryParser.custom "filter-by-facet"
            (List.map
                (ElmParser.run elmParserFilter
                    >> Result.toMaybe
                )
                >> Maybe.Extra.values
                >> List.map (Tuple.mapFirst Aspect.fromString)
                >> Selection.facetFiltersFromList
            )
        )
        (QueryParser.int "offset"
            |> queryParserWithDefault 0
        )
        (QueryParser.int "limit"
            |> queryParserWithDefault Route.defaultLimit
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


elmParserFilter : ElmParser.Parser ( String, String )
elmParserFilter =
    let
        isAspectNameCharacter c =
            Char.isAlphaNum c || c == '_' || c == '.' || c == '-'

        isValueCharacter =
            always True
    in
    ElmParser.succeed Tuple.pair
        |= ElmParser.variable
            { start = isAspectNameCharacter
            , inner = isAspectNameCharacter
            , reserved = Set.empty
            }
        |. ElmParser.symbol ":"
        -- TODO: Review the following code. Seems a bit too permissive.
        --       But note, that the URL design may change anyway.
        |= ElmParser.oneOf
            [ ElmParser.succeed ""
                |. ElmParser.end
            , ElmParser.succeed identity
                |= ElmParser.variable
                    { start = isValueCharacter
                    , inner = isValueCharacter
                    , reserved = Set.empty
                    }
                |. ElmParser.end
            ]


{-| -}
toString : Route -> String
toString route =
    Builder.absolute
        (case route.path of
            NoId ->
                []

            OneId id ->
                [ id |> Id.toString ]

            TwoIds id1 id2 ->
                [ id1 |> Id.toString, id2 |> Id.toString ]
        )
        (Maybe.Extra.values
            [ route.parameters.ftsTerm
                |> Maybe.map
                    (Types.SearchTerm.toString
                        >> Builder.string "fts-term"
                    )
            , buildParameterIfNotDefault
                (ftsSortingTostring >> Builder.string "fts-sorting")
                Route.defaultFtsSorting
                route.parameters.ftsSorting
            ]
            ++ List.map
                (\( aspect, searchTerm ) ->
                    Builder.string "filter-by-fts"
                        (Aspect.toString aspect ++ ":" ++ Types.SearchTerm.toString searchTerm)
                )
                (Sort.Dict.toList route.parameters.ftsFilters)
            ++ List.map
                (\( aspect, value ) ->
                    Builder.string "filter-by-facet"
                        (Aspect.toString aspect ++ ":" ++ value)
                )
                (Sort.Dict.toList route.parameters.facetFilters)
            ++ Maybe.Extra.values
                [ buildParameterIfNotDefault
                    (Builder.int "offset")
                    0
                    route.parameters.offset
                , buildParameterIfNotDefault
                    (Builder.int "limit")
                    Route.defaultLimit
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
