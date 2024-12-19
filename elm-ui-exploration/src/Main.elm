module Main exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


type alias Message =
    { author : String, time : String, text : String }


messageList : List Message
messageList =
    [ { author = "augustin82"
      , time = "6:09AM"
      , text =
            """
            @gampleman I think you need to `clip` the `scrollable` element, and that element should be larger than its
            parent, which (I think) means that the containing parent should have a fixed width
            """
      }
    , { author = "u0421793"
      , time = "6:22AM"
      , text =
            """
            I've been trying to make a few links on a page in elm and elm-ui but I've not found a way to make it work
            because I haven't found any examples of elm-ui which incorporate an anchor element
            """
      }
    , { author = "augustin82"
      , time = "6:27AM"
      , text =
            """
            @u0421793 what are you looking for exactly? do you have an Ellie where you've tried doing some stuff?
            """
      }
    , { author = "icepac"
      , time = "7:53AM"
      , text =
            """
            Anybody replied to @lango
            """
      }
    ]


channelList : List String
channelList =
    [ "ellie"
    , "elm-dev"
    , "elm-discuss"
    , "elm-format"
    , "elm-ui"
    , "general"
    , "news-and-links"
    ]


main : Html msg
main =
    layout
        [ width fill, height fill ]
    <|
        row [ height fill, width fill ]
            [ channelPanel channelList "elm-ui"
            , chatPanel "elm-ui" messageList
            ]



-- The total number of parts that the parent width is divided into is the sum of all fillPortion part counts across
-- the sibling elements


channelPanel : List String -> String -> Element msg
channelPanel channels activeChannel =
    let
        activeChannelAttrs =
            [ Background.color <| rgb255 117 179 201, Font.bold ]

        channelAttrs =
            [ paddingXY 15 5, width fill ]

        channelEl channel =
            el
                (if channel == activeChannel then
                    activeChannelAttrs ++ channelAttrs

                 else
                    channelAttrs
                )
            <|
                text ("# " ++ channel)
    in
    column
        [ height fill
        , width <| fillPortion 1
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
    <|
        List.map channelEl channels


chatPanel : String -> List Message -> Element msg
chatPanel channel messages =
    let
        header =
            row
                [ width fill
                , paddingXY 20 5
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 200 200 200
                ]
                [ el [] <| text ("#" ++ channel)
                , Input.button
                    [ padding 5
                    , alignRight
                    , Border.width 1
                    , Border.rounded 3
                    , Border.color <| rgb255 200 200 200
                    ]
                    { onPress = Nothing
                    , label = text "Search"
                    }
                ]

        messageEntry message =
            column [ width fill, spacingXY 0 5 ]
                [ row [ spacingXY 10 0 ]
                    [ el [ Font.bold ] <| text message.author, text message.time ]
                , paragraph [] [ text message.text ]
                ]

        messagePanel =
            column [ padding 10, spacingXY 0 20, scrollbarY ] <|
                List.map messageEntry messages

        footer =
            el [ alignBottom, padding 20, width fill ] <|
                row
                    [ spacingXY 2 0
                    , width fill
                    , Border.width 2
                    , Border.rounded 4
                    , Border.color <| rgb255 200 200 200
                    ]
                    [ el
                        [ padding 5
                        , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
                        , Border.color <| rgb255 200 200 200
                        , mouseOver [ Background.color <| rgb255 86 182 139 ]
                        ]
                      <|
                        text "+"
                    , el [ Background.color <| rgb255 255 255 255 ] none
                    ]
    in
    column [ height fill, width <| fillPortion 5 ]
        [ header
        , messagePanel
        , footer
        , textWithAlpha
        ]



{-
   Layout basics
    * el - the basic building block of layouts with one child element (often text)
    * text - a text element without wrapping
    * none - an empty element

   To group elements together, you can use:

       * row - arranges its child elements side by side
       * column - stacks its child elements vertically
       * wrappedRow - arranges its child elements side by side but wraps them if they use up too much space(similar to
         inline-block in CSS).

   Padding and spacing
       You can specify internal padding for an element, and you can also specify the spacing between its child elements.
       The padding attribute can be specified in three ways:

       * padding pads the content of an element with a given number of pixels on each side.
       * paddingXY allows you to specify horizontal and vertical padding separately
       * paddingEach is for when  you need different paddings on each side.

   For spacing between elements, in most cases you just use spacing but with layouts like wrappedRow and textColumn,
   you may want to set different horizontal and vertical spacing with spacingXY.
   There is als the spaceEvenly attribute, which will distribute the children within the parent element with equal spacing
   between them.

   Sizing

   There are several ways of specifying element dimensions:
    * px for a fixed number of pixels
    * fill to fill the available space or share it evenly with other elements set to fill space
    * fillPortion to fill the specified portion of element
    * shrink to make the size of the element match the size of its contents.

    Child elements can overflow the parent. Child element's don't expand the dimensions of the parent. Additionally, you
    can constrain width and height by using maximum and minimum functions.

    Transparency
    Sometimes, you may need to hide an element without affecting the surrounding layout. In that case, you can use the
    transparent attribute. A transparent element is supposed to stop receiving input, however as of v1.1.7, due to a bug
    the element continues to receive input, so you need to disable input handling yourself, eg by setting onPress=Nothing.

    It's also possible to set the opacity of an element using the alpha attribute.

-}


transparentButton : Element msg
transparentButton =
    Input.button
        [ transparent True
        , padding 20
        , Border.width 2
        , Border.color <| rgb255 0x50 0x50 0x50
        ]
        { onPress = Nothing
        , label = text "Button"
        }


textWithAlpha : Element msg
textWithAlpha =
    column [ width fill ]
        [ el [ width fill, height <| px 30, Background.color bluish ] <|
            text "First element"
        , el [ alpha 0.5, width fill, height <| px 30, Background.color bluish ] <|
            text "Second element - alpha 0.5"
        , el [ width fill, height <| px 30, Background.color bluish ] <|
            text "Third element"
        ]


bluish : Color
bluish =
    rgb255 0 125 220


exampleLayout =
    --Center in available space
    row [ width fill ]
        [ el [] <| text "no align"
        , el [ centerX ] <| text "centerX"
        , el [] <| text "no align"
        , el [] <| text "no align"
        ]


box : List (Attribute msg) -> Element msg -> Element msg
box attrs elem =
    row (attrs ++ [ width fill ]) [ elem ]


exampleLayout1 =
    --Center relative to the parent element
    row [ width fill ]
        [ el [ width <| fillPortion 2 ] <| box [] <| text "no align"
        , el [ width <| fillPortion 1 ] <| box [ centerX ] <| text "centerX"
        , row [ width <| fillPortion 2 ]
            [ box [ alignRight ] <| text "alignRight"
            , box [] <| text "no align"
            ]
        ]
