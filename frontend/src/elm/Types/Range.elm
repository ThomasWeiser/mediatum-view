module Types.Range exposing
    ( Range(..)
    , compare
    , fromMaybe
    , fromTo
    , isWithin
    , normalize
    , toMaybe
    , unwrap
    )


type Range comparable
    = From comparable
    | To comparable
    | FromTo comparable comparable


normalize : Range comparable -> Range comparable
normalize r =
    case r of
        From _ ->
            r

        To _ ->
            r

        FromTo f t ->
            if f <= t then
                r

            else
                FromTo t f


fromTo : ( comparable, comparable ) -> Range comparable
fromTo ( f, t ) =
    if f <= t then
        FromTo f t

    else
        FromTo t f


fromMaybe : ( Maybe comparable, Maybe comparable ) -> Maybe (Range comparable)
fromMaybe ( mf, mt ) =
    case ( mf, mt ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Just f, Nothing ) ->
            Just (From f)

        ( Nothing, Just t ) ->
            Just (To t)

        ( Just f, Just t ) ->
            if f <= t then
                Just (FromTo f t)

            else
                Just (FromTo t f)


toMaybe : Range comparable -> ( Maybe comparable, Maybe comparable )
toMaybe r =
    case r of
        From f ->
            ( Just f, Nothing )

        To t ->
            ( Nothing, Just t )

        FromTo f t ->
            ( Just f, Just t )


unwrap : a -> (comparable -> a) -> Range comparable -> ( a, a )
unwrap default mapping r =
    case r of
        From f ->
            ( mapping f, default )

        To t ->
            ( default, mapping t )

        FromTo f t ->
            ( mapping f, mapping t )


compare : Range comparable -> Range comparable -> Order
compare rL rR =
    case ( rL, rR ) of
        ( From fL, From fR ) ->
            Basics.compare fL fR

        ( From _, To _ ) ->
            GT

        ( From fL, FromTo fR _ ) ->
            Basics.compare fL fR |> ifEq GT

        ( To _, From _ ) ->
            LT

        ( To tL, To tR ) ->
            Basics.compare tL tR

        ( To _, FromTo _ _ ) ->
            LT

        ( FromTo fL _, From fR ) ->
            Basics.compare fL fR |> ifEq LT

        ( FromTo _ _, To _ ) ->
            GT

        ( FromTo fL tL, FromTo fR tR ) ->
            Basics.compare fL fR |> ifEq (Basics.compare tL tR)


ifEq : Order -> Order -> Order
ifEq secondaryOrder primaryOrder =
    case primaryOrder of
        EQ ->
            secondaryOrder

        _ ->
            primaryOrder


isWithin : Range comparable -> comparable -> Bool
isWithin r v =
    case r of
        From f ->
            v >= f

        To t ->
            v <= t

        FromTo f t ->
            v >= f && v <= t
