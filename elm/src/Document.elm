module Document exposing (Document, Attribute, view)

import Regex
import Html exposing (Html)
import Html.Attributes


type alias Document =
    { metadatatypeName : String
    , attributes : List Attribute
    }


type alias Attribute =
    { field : String
    , name : String
    , width : Int
    , value : Maybe String
    }


view : Document -> Html msg
view document =
    Html.div [ Html.Attributes.class "document" ]
        [ Html.div [ Html.Attributes.class "metadatatype" ]
            [ Html.text document.metadatatypeName ]
        , Html.div [ Html.Attributes.class "attributes" ]
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
        (case attribute.value of
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
        )
