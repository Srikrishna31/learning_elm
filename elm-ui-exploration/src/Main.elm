module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)



{-
   Element msg values are the main building block returned by elm-ui functions. It can be thought of an analogous to a
   div in HTML. Each element can have a number of attributes (Attribute msg values), aside from text element and empty
   element.
   el returns an Element with a single child element.
   Elements are composed with functions like paragraph which takes List (Element msg) and returns another Element msg.
   The paragraph function arranges its child elements inline.
-}


menu : Element msg
menu =
    row
        [ width fill
        , padding 20
        , spacing 20
        ]
        [ el
            -- "logo" element
            [ width <| px 80
            , height <| px 40
            , Border.width 2
            , Border.rounded 6
            , Border.color <| rgb255 0xC0 0xC0 0xC0
            ]
            none
        , el [ alignRight ] <| text "Services"
        , el [ alignRight ] <| text "About"
        , el [ alignRight ] <| text "Contact"
        ]


main =
    layout
        [ width fill, height fill, inFront menu ]
    <|
        el [ centerX, centerY, padding 50 ] <|
            paragraph
                [ Font.size 48, Font.center ]
                [ text "Welcome to"
                , el [ Font.italic ] <| text "this"
                , text " page!"
                ]
