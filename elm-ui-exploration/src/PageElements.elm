module PageElements exposing (..)

import Colors exposing (blue)
import Element exposing (Attribute, Element, download, downloadAs, image, link, newTabLink, text)
import Element.Font as Font



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


linkWithImage : Element msg
linkWithImage =
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
