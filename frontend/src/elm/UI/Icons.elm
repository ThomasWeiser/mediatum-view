module UI.Icons exposing (definitions, icons)

{-|

@docs definitions, icons

-}

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


{-| -}
definitions : Html msg
definitions =
    let
        symbolWithPath : String -> String -> Svg msg
        symbolWithPath name path =
            Svg.symbol
                [ Svg.Attributes.id ("icon-" ++ name)
                , Svg.Attributes.viewBox "0 0 32 32"
                ]
                [ Svg.path [ Svg.Attributes.d path ] []
                ]
    in
    Svg.svg
        [ Svg.Attributes.display "none" ]
        [ Svg.defs
            [ Svg.Attributes.class "svg-icon-definitions" ]
            [ symbolWithPath "expando" "M 6 4 l 20 12 l -20 12 z"
            , symbolWithPath "leaf" "M 6 6 h 20 v 20 h -20 z"
            , symbolWithPath "search" "M19.196 21.854c-1.687 1.202-3.751 1.909-5.98 1.909-5.697 0-10.316-4.619-10.316-10.316s4.619-10.316 10.316-10.316c5.697 0 10.316 4.619 10.316 10.316 0 2.229-0.707 4.293-1.909 5.98l6.778 6.778c0.668 0.668 0.662 1.736-0 2.399l-0.028 0.028c-0.661 0.661-1.736 0.663-2.399 0l-6.778-6.778zM13.217 21.336c4.357 0 7.889-3.532 7.889-7.889s-3.532-7.889-7.889-7.889c-4.357 0-7.889 3.532-7.889 7.889s3.532 7.889 7.889 7.889v0z"
            , symbolWithPath "clear" "M 2 2 L 30 30 M 2 30 L 30 2"
            , symbolWithPath "first" "M 6 2 V 30 M 29,2 15,16 29,30"
            , symbolWithPath "prev" "M 23,2 9,16 23,30"
            , symbolWithPath "next" "M 9,2 23,16 9,30"
            , symbolWithPath "list" "M2,5H6M9,5H30 M2,16H6M9,16H30 M2,27H6M9,27H30"
            , symbolWithPath "reload" "M 16,30 V 2 M 25,21 16,30 7,21"
            ]
        ]


icons =
    { expando = icon "expando"
    , leaf = icon "leaf"
    , search = icon "search"
    , clear = icon "clear"
    , first = icon "first"
    , prev = icon "prev"
    , next = icon "next"
    , list = icon "list"
    , reload = icon "reload"
    , spinner = spinner
    , spinnerSmall = spinnerSmall
    }


icon : String -> Html msg
icon name =
    Svg.svg
        [ Svg.Attributes.class ("svg-icon icon-" ++ name) ]
        [ Svg.use [ Svg.Attributes.xlinkHref ("#icon-" ++ name) ] [] ]


{-| -}
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


{-| Smaller icon for waiting state. Uses CSS animation
-}
spinnerSmall : Html msg
spinnerSmall =
    Html.div
        [ Html.Attributes.class "spinner-small" ]
        []
