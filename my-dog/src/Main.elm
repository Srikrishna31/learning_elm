module Main exposing (main)

import Element
import Element.Background
import Element.Font


main =
    viewLayout


red =
    Element.rgb255 255 0 0


black =
    Element.rgb255 0 0 0


blue =
    Element.rgb255 0 0 200


lightGray =
    Element.rgb255 180 180 180


fontGreatVibes =
    Element.Font.family [ Element.Font.typeface "GreatVibes" ]


fontTypewriter =
    Element.Font.family [ Element.Font.typeface "Typewriter" ]


viewLayout =
    Element.layoutWith
        { options = []
        }
        [ Element.Background.color lightGray
        , Element.padding 22
        ]
        (Element.column []
            [ viewTitle
            , viewSubtitle
            , dogImage
            ]
        )


viewTitle =
    Element.paragraph
        [ Element.Font.bold
        , Element.Font.color blue
        , fontGreatVibes
        , Element.Font.size 48
        ]
        [ Element.text "My Dog"
        ]


viewSubtitle =
    Element.paragraph
        [ Element.Font.color black
        , fontTypewriter
        , Element.Font.size 16
        , Element.paddingXY 0 10
        ]
        [ Element.text "A web page for my dog"
        ]


dogImage =
    Element.image [ Element.width Element.fill ]
        { src = "dog.png"
        , description = "A picture of my dog"
        }
