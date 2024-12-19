module PageElements exposing (..)

import Colors exposing (blue)
import Element exposing (Attribute, Element, centerX, centerY, clip, column, download, downloadAs, el, fill, height, image, link, newTabLink, padding, px, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
