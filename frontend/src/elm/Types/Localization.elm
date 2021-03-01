module Types.Localization exposing
    ( Language(..)
    , languageFromLanguageTag, languageToLanguageTag
    , Translations
    , translation, text, string
    )

{-| Types used for localization of the app.

Currently we offer English and German as UI languages.

@docs Language
@docs languageFromLanguageTag, languageToLanguageTag
@docs Translations
@docs translation, text, string

-}

import Html exposing (Html)


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
translation : Language -> Translations -> String
translation language translations =
    case language of
        LangEn ->
            translations.en

        LangDe ->
            translations.de


{-| -}
string : Config c -> Translations -> String
string config =
    translation config.uiLanguage


{-| -}
text : Config c -> Translations -> Html msg
text config translations =
    Html.text (translation config.uiLanguage translations)


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


languageToLanguageTag : Language -> String
languageToLanguageTag language =
    case language of
        LangEn ->
            "en"

        LangDe ->
            "de"
