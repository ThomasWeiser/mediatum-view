module Types.Localization exposing
    ( Language(..)
    , Translations
    , translation
    )

{-| Types used for localization of the app.

Currently we offer English and German as UI languages.

@docs Language
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
