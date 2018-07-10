module Icons
    exposing
        ( definitions
        , expando
        , leaf
        , search
        , spinner
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
            [ symbolWithPath "expando" "M 6 4 l 20 12 l -20 12 z"
            , symbolWithPath "leaf" "M 6 6 h 20 v 20 h -20 z"
            , symbolWithPath "search" "M19.196 21.854c-1.687 1.202-3.751 1.909-5.98 1.909-5.697 0-10.316-4.619-10.316-10.316s4.619-10.316 10.316-10.316c5.697 0 10.316 4.619 10.316 10.316 0 2.229-0.707 4.293-1.909 5.98l6.778 6.778c0.668 0.668 0.662 1.736-0 2.399l-0.028 0.028c-0.661 0.661-1.736 0.663-2.399 0l-6.778-6.778zM13.217 21.336c4.357 0 7.889-3.532 7.889-7.889s-3.532-7.889-7.889-7.889c-4.357 0-7.889 3.532-7.889 7.889s3.532 7.889 7.889 7.889v0z"
            ]
        ]


expando : Html msg
expando =
    icon "expando"


leaf : Html msg
leaf =
    icon "leaf"


search : Html msg
search =
    icon "search"


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


spinner : Html msg
spinner =
    let
        animate attributeName begin dur values =
            Svg.animate
                [ Svg.Attributes.attributeName attributeName
                , Svg.Attributes.begin begin
                , Svg.Attributes.dur dur
                , Svg.Attributes.values values
                , Svg.Attributes.calcMode "linear"
                , Svg.Attributes.repeatCount "indefinite"
                ]
                []

        circle cx cy r strokeOpacity =
            Svg.circle
                [ Svg.Attributes.cx cx
                , Svg.Attributes.cy cy
                , Svg.Attributes.r r
                , Svg.Attributes.strokeOpacity strokeOpacity
                ]
    in
        Svg.svg
            [ Svg.Attributes.class "spinner"
            , Svg.Attributes.viewBox "0 0 45 45"
            , Svg.Attributes.stroke "black"
            ]
            [ Svg.g
                [ Svg.Attributes.transform "translate(1 1)"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.fillRule "evenodd"
                , Svg.Attributes.strokeWidth "2"
                ]
                [ circle "22" "22" "6" "0" <|
                    [ animate "r" "1.5s" "3s" "6;22"
                    , animate "stroke-opacity" "1.5s" "3s" "1;0"
                    , animate "stroke-width" "1.5s" "3s" "2;0"
                    ]
                , circle "22" "22" "6" "0" <|
                    [ animate "r" "3s" "3s" "6;22"
                    , animate "stroke-opacity" "3s" "3s" "1;0"
                    , animate "stroke-width" "3s" "3s" "2;0"
                    ]
                , circle "22" "22" "8" "1" <|
                    [ animate "r" "0s" "1.5s" "6;1;2;3;4;5;6"
                    , animate "stroke-width" "0s" "1.5s" "2;0.2;0.5;0.8;1.1;1.4;1.7"
                    ]
                ]
            ]
