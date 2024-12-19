module Main exposing (main)

import Colors exposing (background, blue, bluish, grey, lightCharcoal, lightGrey, preferredBlue, white)
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
        el
            [ centerX
            , centerY
            , above <| box [ centerX ] <| text "above"
            , below <| box [] <| text "below"
            , onRight <| box [ alignTop ] <| text "onRight"
            , onLeft <| box [ centerY ] <| text "onLeft"
            ]
        <|
            box
                [ Border.width 10
                , padding 20
                ]
            <|
                text "Main"



--row [ height fill, width fill ]
--    --[ channelPanel channelList "elm-ui"
--    --, chatPanel "elm-ui" messageList
--    [ elementBehindExample
--    ]
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
        , Background.color lightCharcoal
        , Font.color white
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
                    , Border.color lightGrey
                    ]
                    [ el
                        [ padding 5
                        , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
                        , Border.color lightGrey
                        , mouseOver [ Background.color <| rgb255 86 182 139 ]
                        ]
                      <|
                        text "+"
                    , el [ Background.color white ] none
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
        , Border.color grey
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



{-
   Alignment

   For horizontal alignment, we have:

   * alignLeft
   * centerX
   * alignRight

   For vertical alignment, we have:

   * alignTop
   * centerY
   * alignBottom

   An element can have both horizontal and vertical alignment. An element with an alignment towards one of the sides of
   the parent (eg. alignRight) will push other elements which are closer to that boundary.
   Centering an element horizontally or vertically means centering it in the available space rather than in the parent
   container.

   Centering relative to the parent element requires an extra level of nesting. First you have to break up the elements
   into groups which are sized symmetrically around the central element. Then, you can align the elements inside those
   groups as appropriate, with centerX applied to the element inside the central container.
-}


exampleLayout =
    --Center in available space
    row [ width fill ]
        [ el [] <| text "no align"
        , el [ centerX ] <| text "centerX"
        , el [] <| text "no align"
        , el [] <| text "no align"
        ]


box : List (Attribute msg) -> Element msg -> Element msg
box attrs =
    el <|
        [ Background.color white
        , padding 10
        , Border.rounded 6
        , Border.width 3
        , Border.color preferredBlue
        ]
            ++ attrs


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



{-
   Placing elements near, above or below other elements


-}


elementBehindExample =
    el
        [ inFront <|
            el
                [ centerX
                , centerY
                , padding 20
                , Border.width 4
                , Border.color blue
                , Border.rounded 6
                , Background.color white
                ]
            <|
                text "inFront"
        , behindContent <|
            el
                [ alignTop
                , height fill
                , width <| px 200
                , Background.color white
                ]
                none
        , centerX
        , centerY
        , Border.width 10
        , Border.color blue
        , Border.rounded 6
        , Background.color background
        ]
    <|
        paragraph [] [ text sampleText ]


sampleText : String
sampleText =
    """
    The quick, brown fox jumps over a lazy dog. DJs flock by when MTV ax quiz 
 
    prog. Junk MTV quiz graced by fox whelps. Bawds jog, flick quartz, vex 

    nymphs. Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived 

    waltz. Brick quiz whangs jumpy vex fraught vixens jump; dozy fowl quack. 

    Quick wafting zephyrs vex bold Jim; zephyrs blow, vexing daft Jim. Sex- 

    charged fop blew my junk TV quiz. How quickly daft jumping zebras vex. Two 

    driven jocks help fax my big quiz. Quick, Baz, get my woven flax jodhpurs! 

    "Now fax quiz Jack!" my brave ghost pled.  

    """
