module Main exposing (main, shrinkText)

import Html exposing (text)


main : Html.Html msg
main =
    text (shrinkText 120 "Hello Fuzz this is a longer string")


shrinkText : Int -> String -> String
shrinkText max text =
    if max < 0 || String.length text <= max then
        text

    else
        let
            extra =
                String.length text - max
        in
        String.dropRight extra text ++ " ..."
