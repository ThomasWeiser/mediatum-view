module UI.Widgets.ThumbnailSwitch exposing (view)

{-| Small widget for switching and displaying current uiLanguage

@docs view

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Types.Config exposing (Config)
import Types.Localization as Localization


{-| -}
view : Config -> Bool -> (Bool -> msg) -> Html msg
view config state msgFromState =
    Html.span
        [ Html.Events.onClick (msgFromState (not state))
        , Html.Attributes.class "text-button"
        ]
        [ Localization.text config <|
            if state then
                { en = "Show Thumbnails"
                , de = "Vorschaubilder anzeigen"
                }

            else
                { en = "Hide Thumbnails"
                , de = "Vorschaubilder ausblenden"
                }
        ]
