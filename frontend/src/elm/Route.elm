module Route exposing (Route, RouteParameters, RoutePath(..), parseUrl, toString)

import Browser.Navigation
import Maybe.Extra
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

    {-
       , ftsSorting : Maybe RouteFtsSorting
       , filterByYear : Maybe Int Int
    -}
    }


emptyParameters : RouteParameters
emptyParameters =
    { ftsTerm = Nothing
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
    QueryParser.map RouteParameters
        (QueryParser.string "fts-term")


parseUrl : Url -> Maybe Route
parseUrl url =
    Parser.parse parser url


toString : Route -> String
toString page =
    "TODO"



{-
   let
       pieces =
           case page of
               Home ->
                   []

               NodeId id ->
                   [ String.fromInt id ]

               Invalid _ ->
                   []
   in
   Builder.absolute pieces []
-}
