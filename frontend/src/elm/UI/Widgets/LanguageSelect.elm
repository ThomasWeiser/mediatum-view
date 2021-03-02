module UI.Widgets.LanguageSelect exposing (view)

{-|

@docs Context
@docs view

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Types.Localization as Localization exposing (Language)


{-| -}
view : Language -> (Language -> msg) -> Html msg
view uiLanguage msgFromLanguage =
    Html.span [ Html.Attributes.class "language-select" ]
        [ Html.span
            [ Html.Events.onClick (msgFromLanguage Localization.LangDe)
            , Html.Attributes.class "language-option"
            , Html.Attributes.classList
                [ ( "selected", uiLanguage == Localization.LangDe ) ]
            ]
            [ Html.text "de" ]
        , Html.text " | "
        , Html.span
            [ Html.Events.onClick (msgFromLanguage Localization.LangEn)
            , Html.Attributes.class "language-option"
            , Html.Attributes.classList
                [ ( "selected", uiLanguage == Localization.LangEn ) ]
            ]
            [ Html.text "en" ]
        ]
