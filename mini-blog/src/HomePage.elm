module HomePage exposing (..)

import Element
import Element.Font


view : () -> Element.Element msg
view _ =
    Element.column [ Element.padding 20 ]
        [ Element.text "My Blog"
        , Element.paragraph
            [ Element.paddingXY 0 20
            , Element.Font.size 14
            ]
            [ Element.text "Welcome to my mini blog made with Elm and Elm UI"
            ]
        ]
