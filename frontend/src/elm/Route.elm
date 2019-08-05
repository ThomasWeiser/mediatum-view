module Route exposing (Route(..), parseUrl, toString)

import Browser.Navigation
import Maybe.Extra
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | NodeId Int
    | Invalid String


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map NodeId Parser.int
        ]


parseUrl : Url -> Route
parseUrl url =
    Parser.parse parser url
        |> Maybe.Extra.unpack
            (\_ ->
                Invalid ("Invalid URL path: " ++ url.path)
            )
            identity


toString : Route -> String
toString page =
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
