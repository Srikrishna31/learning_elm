module Utils exposing (..)

import Element exposing (Element, fill, height, layout, padding, px, scale, width)
import Html exposing (Html)


generalLayout : Element msg -> Html msg
generalLayout =
    layout [ width fill, height fill ]


layoutWithPadding : Element msg -> Html msg
layoutWithPadding =
    layout [ width fill, padding 50, height fill ]


scaledLayoutWithFixedWidth : Element msg -> Html msg
scaledLayoutWithFixedWidth =
    layout
        [ width <| px 800, padding 50, height fill, scale 0.5 ]
