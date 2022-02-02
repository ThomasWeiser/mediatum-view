module UI.Widgets.LanguageSelect exposing (view)

{-| Small widget for switching and displaying current uiLanguage

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
        [ Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick (msgFromLanguage Localization.LangDe)
            , Html.Attributes.class "text-button language-option"
            , Html.Attributes.classList
                [ ( "selected", uiLanguage == Localization.LangDe ) ]
            ]
            [ Html.text "de" ]
        , Html.text " | "
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick (msgFromLanguage Localization.LangEn)
            , Html.Attributes.class "text-button language-option"
            , Html.Attributes.classList
                [ ( "selected", uiLanguage == Localization.LangEn ) ]
            ]
            [ Html.text "en" ]
        ]
