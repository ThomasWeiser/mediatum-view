module Document exposing
    ( Attribute
    , Document
    , DocumentId
    , idToInt
    , init
    , view
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Regex



{- Actually, the type should be defined like this:

       type DocumentId = DocumentId Int

   But in Elm 0.18 union types are not comparable and therefore not usable as keys of a dict.
   Only ints, floats, chars, strings, lists, and tuples are comparable.
   So, as a workaround we use a tuple with some contrived structure to make it somewhat unique.
-}


type alias DocumentId =
    ( Float, Int )


idToInt : DocumentId -> Int
idToInt ( _, i ) =
    i


idFromInt : Int -> DocumentId
idFromInt idAsInt =
    ( 0.0, idAsInt )


type alias Document =
    { id : DocumentId
    , name : String
    , metadatatypeName : String
    , attributes : List Attribute
    }


type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }


init : Int -> String -> String -> List Attribute -> Document
init idAsInt metadatatypeName name attributes =
    { id = idFromInt idAsInt
    , name = name
    , metadatatypeName = metadatatypeName
    , attributes = attributes
    }


view : (DocumentId -> msg) -> Maybe Int -> Document -> Html msg
view clickMsg maybeNumber document =
    Html.div [ Html.Attributes.class "document" ]
        [ Html.div [ Html.Attributes.class "metadatatype" ]
            [ case maybeNumber of
                Just number ->
                    Html.span [ Html.Attributes.class "result-number" ]
                        [ Html.text <| toString number ++ ". " ]

                Nothing ->
                    Html.text ""
            , Html.span [ Html.Attributes.class "metadatatype" ]
                [ Html.text document.metadatatypeName ]
            ]
        , Html.div
            [ Html.Attributes.class "attributes"
            , Html.Events.onClick (clickMsg document.id)
            ]
            (List.map
                viewAttribute
                document.attributes
            )
        ]


maxAttributeStringLength : Int
maxAttributeStringLength =
    80


viewAttribute : Attribute -> Html msg
viewAttribute attribute =
    let
        isField regex =
            Regex.contains (Regex.regex regex) attribute.field
    in
    case attribute.value of
        Just valueLong ->
            let
                value =
                    if String.length valueLong > maxAttributeStringLength then
                        String.left (maxAttributeStringLength - 3) valueLong ++ "..."

                    else
                        valueLong
            in
            Html.span
                [ Html.Attributes.classList
                    [ ( "attribute", True )
                    , ( "author", isField "author" )
                    , ( "title"
                      , isField "title"
                            && not (isField "congress|journal")
                      )
                    ]
                , Html.Attributes.title (attribute.name ++ ": " ++ valueLong)
                ]
                [ Html.text <|
                    if isField "year" then
                        String.left 4 value ++ ". "

                    else if isField "author" then
                        value ++ ": "

                    else if isField "title|type" then
                        value ++ ". "

                    else
                        attribute.name ++ ": " ++ value ++ ". "
                ]

        Nothing ->
            Html.text ""
