module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, img, text)
import Html.Attributes exposing (class, src)


{-| A compiled Elm application creates a global Elm namespace variable. The Elm variable has properties for any top-level
modules compiled.
Every compiled module has an init function that accepts a configuration object. The node property of the configuration
object specifies a DOM node.
-}
main : Html msg
main =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto (baseUrl ++ "1.jpg") "Surfing"
            , viewDetailedPhoto (baseUrl ++ "2.jpg") "The Fox"
            , viewDetailedPhoto (baseUrl ++ "3.jpg") "Evening"
            ]
        ]


viewDetailedPhoto : String -> String -> Html msg
viewDetailedPhoto url caption =
    div [ class "detailed-photo" ]
        [ img [ src url ] []
        , div [ class "photo-info" ]
            [ h2 [ class "caption" ] [ text caption ] ]
        ]


baseUrl : String
baseUrl =
    "https://programming-elm.com/"
