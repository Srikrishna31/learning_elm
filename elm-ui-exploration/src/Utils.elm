module Utils exposing (..)

import Element exposing (Element, fill, height, layout, padding, px, scale, width)
import Html exposing (Html)


generalLayout : Element msg -> Html msg
generalLayout =
    layout [ width fill, height fill ]


layoutWithPadding : Element msg -> Html msg
layoutWithPadding =
    layout [ width fill, padding 50, height fill ]


layoutWithFixedWidthAndPadding : Element msg -> Html msg
layoutWithFixedWidthAndPadding =
    layout [ width <| px 800, padding 50, height fill ]


scaledLayoutWithFixedWidth : Element msg -> Html msg
scaledLayoutWithFixedWidth =
    layout
        [ width <| px 800, padding 50, height fill, scale 0.5 ]


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


moreSampleText : String
moreSampleText =
    """
    Do greatest at in learning steepest. Breakfast extremity suffering one who all
    all otherwise suspected. He at no nothing forbade up moments. Wholly uneasy at
    missed be of pretty whence. John way sir high than law who week. Surrounded
    prosperous introduced it if is up dispatched. Improved so strictly produced answered
    elegance is.
    """
