module Document exposing
    ( Attribute
    , Document
    , DocumentId
    , attributeValue
    , idToInt
    , idToString
    , init
    , view
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Regex



{- Actually, the type should be defined like this:

       type DocumentId = DocumentId Int

   But in Elm 0.19 union types are not comparable and therefore not usable as keys of a dict.
   Only ints, floats, chars, strings, lists, and tuples are comparable.
   So, as a workaround we use a tuple with some contrived structure to make it somewhat unique.
-}


type alias DocumentId =
    ( Float, Int )


idFromInt : Int -> DocumentId
idFromInt idAsInt =
    ( 0.0, idAsInt )


idToInt : DocumentId -> Int
idToInt ( _, i ) =
    i


idToString : DocumentId -> String
idToString ( _, i ) =
    String.fromInt i


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


attributeValue : String -> Document -> Maybe String
attributeValue key document =
    List.Extra.find
        (\attribute -> attribute.field == key)
        document.attributes
        |> Maybe.map (.value >> Maybe.withDefault "")


view : (DocumentId -> msg) -> Maybe Int -> Document -> Html msg
view clickMsg maybeNumber document =
    Html.div [ Html.Attributes.class "document" ]
        [ Html.div [ Html.Attributes.class "metadatatype" ]
            [ case maybeNumber of
                Just number ->
                    Html.span [ Html.Attributes.class "result-number" ]
                        [ Html.text <| String.fromInt number ++ ". " ]

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
        isField regexString =
            Regex.contains
                (Maybe.withDefault Regex.never (Regex.fromString regexString))
                attribute.field
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
