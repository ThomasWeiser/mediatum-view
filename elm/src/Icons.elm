module Icons
    exposing
        ( definitions
        , collapsed
        , expanded
        , leaf
        )

import Svg exposing (Svg)
import Svg.Attributes
import Html exposing (Html)


definitions : Html msg
definitions =
    Svg.svg
        [ Svg.Attributes.display "none" ]
        [ Svg.defs
            [ Svg.Attributes.class "svg-icon-definitions" ]
            [ symbolWithPath "collapsed" "M 6 4 l 20 12 l -20 12 z"
            , symbolWithPath "expanded" "M 4 8 l 12 20 l 12 -20 z"
            , symbolWithPath "leaf" "M 6 6 h 20 v 20 h -20 z"
            ]
        ]


collapsed : Html msg
collapsed =
    icon "collapsed"


expanded : Html msg
expanded =
    icon "expanded"


leaf : Html msg
leaf =
    icon "leaf"


symbolWithPath : String -> String -> Svg msg
symbolWithPath name path =
    Svg.symbol
        [ Svg.Attributes.id ("icon-" ++ name)
        , Svg.Attributes.viewBox "0 0 32 32"
        ]
        [ Svg.path [ Svg.Attributes.d path ] []
        ]


icon : String -> Html msg
icon name =
    Svg.svg
        [ Svg.Attributes.class "svg-icon" ]
        [ Svg.use [ Svg.Attributes.xlinkHref ("#icon-" ++ name) ] [] ]
