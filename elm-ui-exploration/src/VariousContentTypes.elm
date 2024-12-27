module VariousContentTypes exposing (..)

import Element exposing (Color, el, fill, fromRgb, height, none, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Keyed as Keyed
import Html exposing (Html)
import Palette.Cubehelix as Palette
import SolidColor exposing (SolidColor)
import Utils exposing (layoutWithPadding)



{-
   Performance Optimization

   Element.Keyed allows you to provide a string identifier for child elements, which makes it possible for the diffing
   algorithm to reuse nodes between renders. This can improve performance in situations where child elements are added,
   removed or reordered (typically in a list of some kind).

   Element.Keyed provides three functions:

   el: List (Attribute msg) -> (String, Element msg) -> Element msg
   column: List (Attribute msg) -> List (String, Element msg) -> Element msg
   row: List (Attribute msg) -> List (String, Element msg) -> Element msg

   The difference with their regular counterparts from the Element model is that they take tuples of (String, Element msg).
   You have to ensure that string IDs are unique.

   Note that there are no keyed versions of other container elements like table or wrappedRow.
-}


colorList : Bool -> Html msg
colorList isReversed =
    layoutWithPadding <|
        let
            colorRow clr =
                -- Each row is associated with an ID which is the hex value of the colour
                ( SolidColor.toHex clr
                  --uniqueID made from color value
                , row [ width fill, spacing 20 ]
                    --associated element
                    [ el
                        [ width fill
                        , height fill
                        , Background.color <| solidColorAsColor clr
                        ]
                        none
                    , el [ width fill ] <| text <| SolidColor.toHex clr
                    ]
                )
        in
        Keyed.column [ width fill, spacing 30 ] <|
            -- The children are passed in as a list of id + element tuples
            ( "Header"
            , row [ width fill, spacing 20, Font.bold ]
                [ el [ width fill ] <| text "Color"
                , el [ width fill ] <| text "Value"
                ]
            )
                :: (List.map colorRow <| palette isReversed)


solidColorAsColor : SolidColor -> Color
solidColorAsColor clr =
    let
        ( r, g, b ) =
            SolidColor.toRGB clr
    in
    fromRgb <| { red = r / 255, green = g / 255, blue = b / 255, alpha = 1.0 }


palette : Bool -> List SolidColor
palette isReversed =
    Palette.generate 256
        |> List.sortBy
            (if isReversed then
                SolidColor.toHex >> String.reverse

             else
                SolidColor.toHex
            )
