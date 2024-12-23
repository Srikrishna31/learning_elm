module Text exposing (..)

import Colors exposing (blue)
import Element exposing (alignLeft, column, el, height, image, paragraph, px, spacing, text, width)
import Element.Font as Font
import Html exposing (Html)
import Utils exposing (layoutWithFixedWidthAndPadding, layoutWithPadding, moreSampleText, sampleText)


fontStyling : Html msg
fontStyling =
    layoutWithPadding <|
        paragraph []
            [ el [ Font.color blue ] <| text "Font "
            , el [ Font.italic ] <| text "styling "
            , el [ Font.strike ] <| text "cannot"
            , text " "
            , el [ Font.underline ] <| text "can"
            , text " "
            , el [ Font.regular ] <| text "be "
            , el [ Font.heavy ] <| text "adjusted"
            ]


fontWeights : Html msg
fontWeights =
    layoutWithPadding <|
        paragraph []
            [ el [ Font.hairline ] <| text "Hairline"
            , text "\n"
            , el [ Font.extraLight ] <| text "Extra Light"
            , text "\n"
            , el [ Font.light ] <| text "Light"
            , text "\n"
            , el [ Font.regular ] <| text "Regular"
            , text "\n"
            , el [ Font.medium ] <| text "Medium"
            , text "\n"
            , el [ Font.semiBold ] <| text "Semi-Bold"
            , text "\n"
            , el [ Font.bold ] <| text "Bold"
            , text "\n"
            , el [ Font.extraBold ] <| text "Extra Bold"
            , text "\n"
            , el [ Font.heavy ] <| text "Heavy"
            ]



{-
   Some fonts can have additional rendering features which can be enabled or disabled selectively.
    * smallCaps renders fonts in uppercase glyphs but at lowercase glyph size
    * ligatures enables ligatures
    * slashedZero changes the zero glyph to have a diagonal slash
    * ordinal renders ordinal markers like 1st, 2nd and so on with special glyphs
    * tabularNumbers makes number glyphs monospaced
    * stackedFractions and diagonalFractions change the rendering of fractions like 1/2 or 5/7
    * swash enables swashes on the glyphs.
-}


fontAttributeStyles : Html msg
fontAttributeStyles =
    layoutWithPadding <|
        column [ spacing 10 ]
            [ el [ Font.glow blue 3 ] <| text "Font.glow"
            , el
                [ Font.size 32
                , Font.shadow { offset = ( 2, 4 ), blur = 3, color = blue }
                ]
              <|
                text "Font.shadow"
            , el [ Font.letterSpacing 4, Font.size 32 ] <|
                text "Font.letterSpacing"
            , el [ Font.wordSpacing 16, Font.size 32 ] <|
                text "Custom word spacing"
            ]



{-
   Text Layout

    Element.paragraph lays out its children as wrapped inline elements, and also gets text in text elements to wrap.
    Line spacing can be adjusted by adding a spacing attribute with a given number of pixels.
-}


textLayout : Html msg
textLayout =
    layoutWithFixedWidthAndPadding <|
        paragraph []
            [ image [ alignLeft, width <| px 200, height <| px 100 ]
                { src = "https://picsum.photos/200/100"
                , description = "Image"
                }
            , image [ alignLeft, width <| px 200, height <| px 100 ]
                { src = "https://picsum.photos/200/100"
                , description = "Image"
                }
            , text "Paragraph with images"
            , el [ Font.color blue ] <| text sampleText
            , image [ alignLeft, width <| px 200, height <| px 100 ]
                { src = "https://picsum.photos/200/100"
                , description = "Image"
                }
            , el [ Font.color blue ] <| text moreSampleText
            ]
