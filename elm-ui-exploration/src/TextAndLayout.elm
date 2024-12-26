module TextAndLayout exposing (..)

import Colors exposing (blue, darkCharcoal, lightBlue, lightGrey, white)
import Element exposing (Attribute, Option, alignLeft, alignRight, column, el, fill, focusStyle, focused, height, image, layout, layoutWith, minimum, mouseDown, mouseOver, noStaticStyleSheet, padding, paddingEach, paragraph, px, scrollbarY, spacing, text, textColumn, width)
import Element.Background as Background exposing (color)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, div)
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
   TextAndLayout Layout

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



{-
   textColumn allows you to string paragraphs together.The spacing attribute determines the amount of vertical space
   between paragraphs in this case.

-}


textColumnExample : Html msg
textColumnExample =
    layoutWithFixedWidthAndPadding <|
        textColumn [ width fill, height <| minimum 0 <| px 450, spacing 30, scrollbarY ]
            [ el
                [ alignLeft
                , paddingEach { right = 10, bottom = 10, top = 0, left = 0 }
                ]
              <|
                image [ width <| px 150, height <| px 300 ]
                    { src = "https://picsum.photos/150/300"
                    , description = "Image"
                    }
            , paragraph []
                [ image [ alignRight, width <| px 200, height <| px 100 ]
                    { src = "https://picsum.photos/200/100"
                    , description = "Image"
                    }
                , text "textColumn with images and paragraphs."
                , el [ Font.color blue ] <| text sampleText
                ]
            , paragraph [] [ el [ Font.color blue ] <| text sampleText ]
            , paragraph [] [ el [ Font.color blue ] <| text moreSampleText ]
            ]



{-
      Multiple layouts on a page

   You can't simply add multiple layout calls into a view because each layout produces a global stylesheet, and having
   more than one of them is problematic as styles will clash. For this reason, there should only be a single call to layout
   in a view.
   To resolve this conflict, in addition to the plain layout function, there is also layoutWith which takes a list of
   options, with one of the available options being noStaticStyleSheet.
-}


exampleLayoutWith : Html msg
exampleLayoutWith =
    div []
        [ layout [] <|
            column []
                [ paragraph [ Font.size 28, Font.color blue ]
                    [ text "Layout 1" ]
                , Input.button [] { onPress = Nothing, label = text "Button" }
                ]
        , layoutWith { options = [ noStaticStyleSheet ] } [] <|
            column []
                [ paragraph [ Font.size 28, Font.color darkCharcoal ]
                    [ text "Layout 2" ]
                , Input.button [] { onPress = Nothing, label = text "Button" }
                ]
        ]



{-
      Temporary Styles
   Some styles should only be applied temporarily, when an element is in a particular state:

    * mouseOver (having the mouse pointer over the element)
    * mouseDown (having the mouse pointer over and a mouse button pressed)
    * focused (having the focus for input)

   These three are attribute functions which take a list of decorations as their argument.
   decorations are a subset of attributes which are purely visual and do not affect the layout, with a rather odd exception
   of Font.size

   These are the decorative attributes you can use in temporary styles:

    * Opacity attributes
        - transparent
        - alpha
    * Adjustment attributes
        - moveUp
        - moveDown
        - moveLeft
        - moveRight
        - rotate
        - scale
    * Font attributes
        - color
        - size
        - glow
        - shadow
    * Background attributes
        - color
        - gradient
    * Border attributes
        - glow
        - innerGlow
        - shadow
        - innerShadow

-}


buttonTempStyle : Html msg
buttonTempStyle =
    layoutWithFixedWidthAndPadding <|
        Input.button
            [ padding 10
            , Border.width 3
            , Border.rounded 6
            , Border.color blue
            , Background.color lightBlue
            , mouseOver
                [ Background.color white
                , Border.color lightGrey
                ]
            ]
            { onPress = Nothing, label = text "Launch" }


buttonStyleOnMouseDownAndMouseOver : Html msg
buttonStyleOnMouseDownAndMouseOver =
    layoutWithFixedWidthAndPadding <|
        Input.button
            [ padding 10
            , Border.width 3
            , Border.rounded 6
            , Border.color blue
            , Background.color lightBlue
            , Font.variant Font.smallCaps

            -- The order of mouseDown/mouseOver can be significant when changing the same attribute in both
            , mouseDown
                [ Background.color blue
                , Border.color blue
                , Font.color white
                ]
            , mouseOver
                [ Background.color white
                , Border.color lightGrey
                ]
            ]
            { onPress = Nothing, label = text "Launch" }



{-
   forceHover option (ie. layoutWith {options = [forceHover]} []) makes the decorations in mouseOver apply regardless of
   the hover state of the element. This may be useful for example, for testing purposes, or perhaps if you want these
   visual styles to be switched on for all elements on touch-based devices, where they would never show up otherwise due
   to the absence of a cursor.

   Instead of adding a focused attribute to each control, you can also define a global focus style, once again by passing
   an option called focusStyle to layoutWith.

   The focus style can specify any or all of border color, background color and a shadow, which will apply to all controls
   which display a focus indicator. Individual controls can still override this common style with their own focused
   attribute.
-}


commonFocusStyle : Option
commonFocusStyle =
    focusStyle
        { borderColor = Just darkCharcoal
        , backgroundColor = Nothing
        , shadow = Just { color = blue, offset = ( 4, 4 ), size = 3, blur = 10 }
        }


buttonWithFocusStyle : Html msg
buttonWithFocusStyle =
    layoutWith { options = [ commonFocusStyle ] }
        [ padding 50 ]
    <|
        Input.button
            [ padding 20
            , Background.color lightBlue
            , Border.width 2
            , Border.rounded 16
            , Border.color blue
            , Border.shadow
                { offset = ( 4, 4 ), size = 3, blur = 10, color = lightGrey }
            , Font.color white
            , mouseOver
                [ Background.color white, Font.color darkCharcoal ]
            , focused
                [ Border.shadow
                    { offset = ( 4, 4 ), size = 3, blur = 10, color = blue }
                ]
            ]
            { onPress = Nothing
            , label = text "Button with focus style"
            }
