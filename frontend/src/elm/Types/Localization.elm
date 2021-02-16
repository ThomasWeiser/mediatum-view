module Types.Localization exposing (Language(..), Translations, translation)

{-| -}


type alias Translations =
    { en : String
    , de : String
    }


type Language
    = LangEn
    | LangDe


translation : Language -> Translations -> String
translation language translations =
    case language of
        LangEn ->
            translations.en

        LangDe ->
            translations.de
