module Types.Localization exposing
    ( Language(..)
    , languageFromLanguageTag, languageToLanguageTag
    , Translations
    , translation
    )

{-| Types used for localization of the app.

Currently we offer English and German as UI languages.

@docs Language
@docs languageFromLanguageTag, languageToLanguageTag
@docs Translations
@docs translation

-}


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


{-| -}
translation : Language -> Translations -> String
translation language translations =
    case language of
        LangEn ->
            translations.en

        LangDe ->
            translations.de


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
