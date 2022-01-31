module Types.Localization exposing
    ( Language(..)
    , languageFromLanguageTag, languageToLanguageTag
    , Translations
    , string, text, title
    )

{-| Types used for localization of the app.

Currently we offer English and German as UI languages.

@docs Language
@docs languageFromLanguageTag, languageToLanguageTag
@docs Translations
@docs string, text, title

-}

import Html exposing (Attribute, Html)
import Html.Attributes


{-| A text with translations into the supported languages
-}
type alias Translations =
    { en : String
    , de : String
    }


{-| -}
type Language
    = LangEn
    | LangDe


type alias Config c =
    { c | uiLanguage : Language }


{-| -}
string : Config c -> Translations -> String
string config translations =
    case config.uiLanguage of
        LangEn ->
            translations.en

        LangDe ->
            translations.de


{-| -}
text : Config c -> Translations -> Html msg
text config translations =
    Html.text (string config translations)


{-| -}
title : Config c -> Translations -> Attribute msg
title config translations =
    Html.Attributes.title (string config translations)


{-| -}
languageFromLanguageTag : String -> Maybe Language
languageFromLanguageTag languageTag =
    case String.left 2 languageTag of
        "en" ->
            Just LangEn

        "de" ->
            Just LangDe

        _ ->
            Nothing


{-| -}
languageToLanguageTag : Language -> String
languageToLanguageTag language =
    case language of
        LangEn ->
            "en"

        LangDe ->
            "de"
