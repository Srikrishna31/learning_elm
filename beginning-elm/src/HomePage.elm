module HomePage exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (Regex)


view : String -> Html msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Welcome to Dunder Mifflin!" ]
        , p []
            [ text "Dunder Mifflin Inc. (stock symbol "
            , strong [] [ text "DMI" ]
            , text <|
                """
                ) is a micro-cap regional paper and office
                supply distributor with an emphasis on servicing
                small-business clients.
                """
            ]
        , p []
            [ text <| "Regex.contains regex apollo11 " ++ (fromBool <| Regex.contains regex apollo11) ]
        ]


main : Html msg
main =
    view "dummy model"


pattern : String
pattern =
    "\\d\\d:\\d\\d (a\\.m\\.|p\\.m\\.)"


maybeRegex : Maybe Regex
maybeRegex =
    Regex.fromString pattern


regex : Regex
regex =
    Maybe.withDefault Regex.never maybeRegex


apollo11 : String
apollo11 =
    """
            On July 16, 1969, the massive Saturn V rocket lifted off from NASA's Kennedy Space Center at 09:32 a.m. EDT.
            Four days later, on July 20, Neil Armstrong and BUzz Aldrin landed on the Moon. 
           """


fromBool : Bool -> String
fromBool value =
    if value then
        "True"

    else
        "False"
