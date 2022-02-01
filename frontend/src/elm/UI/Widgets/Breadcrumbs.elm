module UI.Widgets.Breadcrumbs exposing
    ( Context
    , view
    )

{-|

@docs Context
@docs view

-}

import Cache exposing (Cache)
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import RemoteData
import Types.Config exposing (Config)
import Types.Id exposing (FolderId)
import Types.Navigation as Navigation
import Types.Route exposing (Route)
import Utils.List


{-| Any context type that exposes the necessary fields config, cache and route
-}
type alias Context c =
    { c
        | config : Config
        , cache : Cache
        , route : Route
    }


{-| -}
view : Context c -> Maybe (List FolderId) -> Html msg
view context maybeLineage =
    let
        ellipsis =
            Html.text "..."

        separator =
            Html.text " > "
    in
    Html.span [ Html.Attributes.class "breadcrumbs" ]
        (maybeLineage
            |> Maybe.Extra.unwrap
                [ ellipsis ]
                (\lineage ->
                    lineage
                        |> List.reverse
                        |> Utils.List.mapEllipsis
                            ellipsis
                            (\folderId ->
                                Cache.get context.cache.folders folderId
                                    |> RemoteData.toMaybe
                                    |> Maybe.map
                                        (\folder ->
                                            Html.a
                                                [ Navigation.alterRouteHref
                                                    context
                                                    (Navigation.ShowListingWithFolder folderId)
                                                ]
                                                [ Html.text folder.name ]
                                        )
                            )
                        |> List.intersperse separator
                )
        )
