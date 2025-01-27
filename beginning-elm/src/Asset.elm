module Asset exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attr


type Image
    = Image String


waterfall : Image
waterfall =
    image "waterfall.jpg"


image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
