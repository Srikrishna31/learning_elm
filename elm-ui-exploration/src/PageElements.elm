module PageElements exposing (..)

import Colors exposing (black, blue, bluish, borderGrey, checkBoxColor, green, grey, lightBackground, lightGrey, orange, preferredBlue, red, white)
import Element exposing (Attribute, Color, Element, alpha, behindContent, centerX, centerY, clip, column, download, downloadAs, el, fill, fillPortion, focused, height, image, link, mouseOver, newTabLink, none, padding, px, row, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Utils exposing (generalLayout, layoutWithPadding)



{-
   A web page can include elements such as links or images:
       * link and a few specialized versions of links
       * image for displaying images.
       * table for showing tabular data.

   elm-ui also includes versions of the standard browser input controls:
       * button
       * checkbox
       * radio
       * text for a single line text input, as well as a bunch of inputs that work with browser autofill, such as username
       and email
       * multiline for a multiline text input
       * slider

   Note that dropdown isn't in this list. That's likely because dropdowns are often a UI anti-pattern, as they are difficult
   to use and can usually be replaced with a list of selectable options. However, if you need one, you can still implement
   it using the primitives provided by elm-ui, or you could use a package such as PaackEng/elm-ui-dropdown
-}


linkAttrs : List (Attribute msg)
linkAttrs =
    [ Font.bold, Font.underline, Font.color blue ]


styledLink : Element msg
styledLink =
    link linkAttrs
        { url = "https://example.com", label = text "Styled link" }



-- The label can be any element, for example an image:


linkWithImage : Html msg
linkWithImage =
    generalLayout <|
        link []
            { url = "https://example.com"
            , label =
                image []
                    { src = "https://picsum.photos/200/100"
                    , description = "Image link"
                    }
            }



-- There are three special kinds of links. newTabLink opens in a new browser tab:


linkOpeningInNewTab : Element msg
linkOpeningInNewTab =
    newTabLink linkAttrs
        { url = "https://example.com", label = text "New tab link" }



-- download and downloadAs trigger file downloads:


downloadLink : Element msg
downloadLink =
    download linkAttrs
        { url = "/elm.json", label = text "Download file" }


downloadAsLink : Element msg
downloadAsLink =
    downloadAs linkAttrs
        { url = "/elm.json", filename = "renamed.json", label = text "Download renamed file" }



{-
   Images
   An image just needs a source and a description (for assistive technologies).
-}


scaledImage : Html msg
scaledImage =
    generalLayout <|
        image [ width <| px 300, height <| px 200 ]
            { src = "https://picsum.photos/600/400"
            , description = "A 2x image"
            }



{-
   Sometimes images need to be clipped, like in the popular round profile photos. This can be achieved by wrapping an image
   in an element with clipped contents and a circular border:
-}


circularBorderImage : Html msg
circularBorderImage =
    layoutWithPadding <|
        el [ Border.rounded 150, clip ] <|
            image []
                { src = "https://picsum.photos/300/300"
                , description = "Circular image"
                }



{-
   It's also possible to use images as background. The Background module includes several attribute options:
       * Background.image for an images that's resized proportionally to the element, with overlflow cropped
       * Background.uncropped for an image that's resized to fit into the element, without cropping
       * Background.tiled for an image that's tiled both vertically and horizontally
       * Background.tiledX for an image that's tiled horizontally
       * Background.tiledY for an image that's tiled vertically
-}


imageTiles : Html msg
imageTiles =
    layoutWithPadding <|
        column []
            [ el
                [ Background.image "https://picsum.photos/300/300"
                , width fill
                , padding 20
                , Font.size 32
                ]
              <|
                text "Background image"
            , el
                [ Background.uncropped "https://picsum.photos/300/300"
                , width fill
                , padding 20
                , Font.size 32
                ]
              <|
                text "Uncropped background image"
            , el
                [ Background.tiled "https://picsum.photos/100/100"
                , width fill
                , height <| px 200
                , padding 20
                , Font.size 32
                ]
              <|
                el [ centerX, centerY ] <|
                    text "Tiled background image"
            , el
                [ Background.tiledX "https://picsum.photos/100/100"
                , width fill
                , height <| px 200
                , padding 20
                , Border.width 3
                , Font.size 32
                ]
              <|
                el [ centerX, centerY ] <|
                    text "Horizontally tiled background image"
            ]



{-
      Element.table helps you display a list of records in tabular format:

   To create a table you need to provide table with a list of attributes (one useful attribute is spacing as it controls
   spacing between cells), as well as a record with two fields: data and columns. data is a list of records which can have
   many fields. columns is a list of records which define how each column is going to be rendered.

   Each column record needs to have three fields:
    * header which is an Element that will be the column header
    * width which determines the width of the column
    * view which is a function that takes a record from your data list and turns into an Element.

    Sometimes, you might need to know the row number when rendering cells. In that situation, you can use indexedTable.
    The only difference from table is that the view function in column definition takes the row index in addition to the
    record: view: Basics.Int -> record -> Element msg
-}


colors : List { color : Color, name : String }
colors =
    [ { color = black
      , name = "Black"
      }
    , { color = blue
      , name = "Blue"
      }
    , { color = green
      , name = "Green"
      }
    , { color = orange
      , name = "Orange"
      }
    , { color = red
      , name = "Red"
      }
    ]


colorTable : Html msg
colorTable =
    layoutWithPadding <|
        let
            headerAttrs =
                [ Font.bold
                , Font.color blue
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color black
                ]
        in
        table
            [ width shrink
            , spacing 10
            ]
            { data = colors
            , columns =
                [ { header = el headerAttrs <| text "Color Name"
                  , width = fillPortion 2
                  , view = .name >> text >> el [ centerY ]
                  }
                , { header = el headerAttrs <| text "Color Sample"
                  , width = fillPortion 1
                  , view =
                        \rec ->
                            el
                                [ width fill
                                , height <| px 40
                                , Background.color rec.color
                                ]
                                none
                  }
                ]
            }



{-
   Buttons
   A button is created using Input.button, which requires supplying an element for the button label, and an optional
   message handler. elm-ui buttons are implemented with <div> rather than <button>.

   if you set the onPress handler to Nothing, it doesn't disable the button or change it's appearance, it only stops it
   from generating messages. The general recommendation in elm-ui is to avoid disabled controls as much as possible, as
   they can be problematic for accessibility. When a button cannot perform it's normal action, it's recommended to still
   handle clicks but in response explain to the user why the button isn't working.
-}


unstyledButton : Html msg
unstyledButton =
    layoutWithPadding <|
        Input.button [] { onPress = Nothing, label = text "Unstyled button" }


buttonWithFocusStyle : Html msg
buttonWithFocusStyle =
    layoutWithPadding <|
        Input.button
            [ padding 20
            , Border.width 2
            , Border.rounded 16
            , Border.color grey
            , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = grey }
            , Background.color bluish
            , Font.color white
            , mouseOver
                [ Background.color white, Font.color black ]
            , focused
                [ Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = blue } ]
            ]
            { onPress = Nothing
            , label = text "Button with focus style"
            }


imageButton : Html msg
imageButton =
    layoutWithPadding <|
        Input.button
            [ padding 3, Border.rounded 9, Border.width 3 ]
            { onPress = Nothing
            , label =
                el [ clip, Border.rounded 6 ] <|
                    image
                        [ width <| px 200
                        , height <| px 200
                        , mouseOver [ alpha 0.7 ]
                        ]
                        { src = "https://picsum.photos/200/200?grayscale"
                        , description = "Image button"
                        }
            }



{-
   Labels for inputs
   elm-ui requires input controls other than button and link to have labels of type label msg. A label can be places in
   different positions:

       * labelLeft
       * labelRight
       * labelAbove
       * labelBelow

   A label function takes a list of attributes and an element. It's also possible to hide a label using labelHidden instead
   of the functions above. labelHidden takes a String in order to make the label text accessible to screen readers.
   In general, it's recommended to provide a visible label unless the purpose of the input is very clear from the context
   or other clues.
-}


type Msg
    = UserTypedText String
    | UserToggledCheckbox Bool
    | UserChoseDirection Direction
    | UserMovedSlider Float


type alias Model =
    { text : String
    , isChecked : Bool
    , selectedOption : Maybe Direction
    , selectedValue : Float
    }


init : Model
init =
    { text = ""
    , isChecked = True
    , selectedOption = Nothing
    , selectedValue = 0.0
    }


textInputLabel : Html Msg
textInputLabel =
    layoutWithPadding <|
        Input.text []
            { onChange = UserTypedText
            , text = init.text
            , placeholder = Just <| Input.placeholder [] <| text "Type here"
            , label = Input.labelLeft [ centerY ] <| text "Text input"
            }



{-
   Checkboxes
   A checkbox requires storing a Bool in the model.  To create a checkbox with Input.checkbox, you need to pass it a message
   that takes a Bool, an icon, the current value, and a label.
-}


defaultCheckboxView : Html Msg
defaultCheckboxView =
    layoutWithPadding <|
        Input.checkbox [ Font.size 20 ]
            { onChange = UserToggledCheckbox
            , icon = Input.defaultCheckbox
            , checked = init.isChecked
            , label = Input.labelRight [] <| text "Default checkbox"
            }



{-
   You can also supply your own icon in the form of a function that takes a boolean value and returns an element:
-}


yesNoCheckbox : Html Msg
yesNoCheckbox =
    layoutWithPadding <|
        Input.checkbox [ Font.size 48 ]
            { onChange = UserToggledCheckbox
            , icon = checkBoxIcon
            , checked = init.isChecked
            , label = Input.labelRight [] <| text "Yes or no?"
            }


checkBoxIcon : Bool -> Element msg
checkBoxIcon isChecked =
    el
        [ width <| px 40
        , height <| px 40
        , centerY
        , padding 4
        , Border.rounded 6
        , Border.width 2
        , Border.color <| borderGrey
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    checkBoxColor

                else
                    white
            ]
        <|
            none



--Since the label is an arbitrary element, you can easily implement on/off toggles with checkboxes, for example


labelledCheckBox : Html Msg
labelledCheckBox =
    layoutWithPadding <|
        Input.checkbox [ width shrink, Font.size 20 ]
            { onChange = UserToggledCheckbox
            , icon = labelledCheckBoxIcon
            , checked = init.isChecked
            , label = Input.labelHidden "On/Off"
            }


labelledCheckBoxIcon : Bool -> Element msg
labelledCheckBoxIcon isChecked =
    let
        knob =
            el
                [ width <| px 36
                , height <| px 36
                , Border.rounded 18
                , Border.width 2
                , Border.color borderGrey
                , Background.color white
                ]
                none
    in
    el
        [ width <| px 100
        , height <| px 48
        , centerY
        , padding 4
        , Border.rounded 6
        , Border.width 2
        , Border.color <| borderGrey
        , Background.color <|
            if isChecked then
                checkBoxColor

            else
                white
        ]
    <|
        row [ width fill ] <|
            if isChecked then
                [ el [ centerX ] <| text "On", knob ]

            else
                [ knob, el [ centerX ] <| text "Off" ]



{-
   Radio buttons
   Creating radio selectors is quite similar to checkboxes, except you always have to provide a set of options rather than
   creating individual radio buttons:
-}


type Direction
    = Up
    | Down
    | Left
    | Right


simpleRadioButton : Html Msg
simpleRadioButton =
    layoutWithPadding <|
        Input.radio
            [ padding 10
            , spacing 20
            ]
            { onChange = UserChoseDirection
            , selected = init.selectedOption
            , label = Input.labelAbove [] <| text "Radio selection"
            , options =
                [ Input.option Down <| text "Down"
                , Input.option Up <| text "Up"
                , Input.option Left <| text "Left"
                , Input.option Right <| text "Right"
                ]
            }



{-
   Radio buttons can be arranged in a row by switching from Input.radio to Input.radioRow which takes the same arguments.
   You can customize the rendering of radio buttons by using Input.optionWith in place of Input.option:
   Rather than taking an Element msg value as its second argument, Input.optionWith takes a function OptionState -> Element msg.
   An OptionState value can be Idle, Focused or Selected, allowing you to display different states of the radio button.
-}


simpleRowRadioButton : Html Msg
simpleRowRadioButton =
    layoutWithPadding <|
        Input.radioRow
            [ padding 10
            , spacing 30
            ]
            { onChange = UserChoseDirection
            , selected = init.selectedOption
            , label = Input.labelAbove [] <| text "Radio selection with a custom look"
            , options =
                [ Input.optionWith Down <| radioOption "Down"
                , Input.optionWith Up <| radioOption "Up"
                , Input.optionWith Left <| radioOption "Left"
                , Input.optionWith Right <| radioOption "Right"
                ]
            }


radioOption : String -> Input.OptionState -> Element msg
radioOption label state =
    row [ spacing 10 ]
        [ el
            [ width <| px 30
            , height <| px 30
            , centerY
            , padding 4
            , Border.rounded 6
            , Border.width 2
            , Border.color borderGrey
            ]
          <|
            el
                [ width fill
                , height fill
                , Border.rounded 4
                , Background.color <|
                    case state of
                        Input.Idle ->
                            white

                        Input.Focused ->
                            lightBackground

                        Input.Selected ->
                            preferredBlue
                ]
                none
        , text label
        ]



{-
   Single line text inputs

   A single line text input box is made with Input.text:

   The record argument of this function requires a message that takes a String, the current value of the tet in the input,
   an optional placeholder (note that it's an element wrapped in Input.placeholder which can have its own attributes), and
   finally a label.
   There are several specialised input functions which provide a hint to the browser about which kind of input they
   generate so that the browser can autofill them:
    * username
    * currentPassword
    * newPassword
    * email
    * search (a search input field)
    * spellChecked ( a text input that will be spellchecked if spellchecking is available).

    currentPassword and newPassword functions take a record argument which has an extra field (show) in addition to the
    fields required by Input.text. This field indicates whether the password text should be displayed or obscured.

    Multiline text inputs

    A multiline text input is created with Input.multiline in a similar fashion to a single line input:

-}


singleLineInputText : Html Msg
singleLineInputText =
    layoutWithPadding <|
        Input.text []
            { onChange = UserTypedText
            , text = init.text
            , placeholder = Just <| Input.placeholder [] <| text "Type here"
            , label = Input.labelLeft [ centerY ] <| text "Text input"
            }


simpleMultilineTextInput : Html Msg
simpleMultilineTextInput =
    layoutWithPadding <|
        Input.multiline
            [ width <| px 450
            , height <| px 150
            , Border.rounded 6
            , Border.width 2
            , Border.color <| borderGrey
            ]
            { onChange = UserTypedText
            , text = init.text
            , placeholder = Just <| Input.placeholder [] <| text "Type your message"
            , label = Input.labelAbove [] <| text "Message"
            , spellcheck = True
            }



{-
   Sliders
   A slider is a good alternative to a text input for numeric values within a range, particularly floating point values.
   The slider function from the Element.input module lets you create a slider:
-}


simpleSlider : Html Msg
simpleSlider =
    layoutWithPadding <|
        Input.slider
            [ height <| px 30
            , behindContent <|
                --Slider track
                el
                    [ width fill
                    , height <| px 3
                    , centerY
                    , Background.color lightGrey
                    , Border.rounded 2
                    ]
                    Element.none
            ]
            { onChange = UserMovedSlider
            , label =
                Input.labelAbove [] <|
                    text ("Floating point value: " ++ String.fromFloat init.selectedValue)
            , min = 0
            , max = 100
            , step = Nothing
            , value = init.selectedValue
            , thumb = Input.defaultThumb
            }



{-
   It's possible to customize the slider "handle" (called a "thumb" in elm-ui).
   A slider can be either horizontal or vertical. Its orientation can be changed to vertical using a fixed width and height
   fill in the slider attributes, or alternatively a fixed height and width such that height is greater than width.
-}


thumb =
    Input.thumb
        [ width <| px 60
        , height <| px 24
        , Border.width 2
        , Border.rounded 6
        , Background.color white
        ]


verticalSlider : Html Msg
verticalSlider =
    layoutWithPadding <|
        Input.slider
            [ height <| px 300
            , width <| px 30
            , behindContent <|
                -- Slider track
                el
                    [ width <| px 20
                    , height fill
                    , centerX
                    , Background.color lightGrey
                    , Border.rounded 6
                    ]
                    none
            ]
            { onChange = round >> toFloat >> UserMovedSlider
            , label =
                Input.labelAbove [] <| text ("Integer value: " ++ String.fromFloat init.selectedValue)
            , min = 0
            , max = 100
            , step = Just 10
            , value = init.selectedValue
            , thumb = thumb
            }
