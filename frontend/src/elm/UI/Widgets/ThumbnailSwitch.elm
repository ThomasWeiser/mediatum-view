module UI.Widgets.ThumbnailSwitch exposing (view)

{-| Small widget for switching and displaying current uiLanguage

@docs view

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Types.Config exposing (Config)
import Types.Localization as Localization
import UI.Icons


{-| -}
view : Config -> Bool -> (Bool -> msg) -> Html msg
view config state msgFromState =
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Events.onClick (msgFromState (not state))
        , Html.Attributes.class "visual-button"
        , Localization.title config <|
            if state then
                { en = "Show Thumbnails"
                , de = "Vorschaubilder anzeigen"
                }

            else
                { en = "Hide Thumbnails"
                , de = "Vorschaubilder ausblenden"
                }
        ]
        [ UI.Icons.icons.eye (not state) ]
