module Main exposing (main)

import Browser
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Element.Region as ER
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = darkColors
        , view = viewLayout
        , update = update
        }


type Msg
    = MsgChangeColors


update : Msg -> Model -> Model
update _ model =
    if model.primary == darkColors.primary then
        lightColors

    else
        darkColors


darkColors : Model
darkColors =
    { lightGray = E.rgb255 180 180 180
    , primary = E.rgb255 0xFF 0xAB 0x00
    , primaryLight = E.rgb255 0xFF 0xDD 0x4B
    , primaryDark = E.rgb255 0xC6 0x7C 0x00
    , secondary = E.rgb255 0x3E 0x27 0x23
    , secondaryLight = E.rgb255 0x6A 0x4F 0x4B
    , secondaryDark = E.rgb255 0x1B 0x00 0x00
    , textOnPrimary = E.rgb255 0x00 0x00 0x00
    , textOnSecondary = E.rgb255 0xFF 0xFF 0xFF
    }


lightColors : Model
lightColors =
    { lightGray = E.rgb255 180 180 180
    , secondary = E.rgb255 0xFF 0xAB 0x00
    , secondaryLight = E.rgb255 0xFF 0xDD 0x4B
    , secondaryDark = E.rgb255 0xC6 0x7C 0x00
    , primary = E.rgb255 0x3E 0x27 0x23
    , primaryLight = E.rgb255 0x6A 0x4F 0x4B
    , primaryDark = E.rgb255 0x1B 0x00 0x00
    , textOnSecondary = E.rgb255 0x00 0x00 0x00
    , textOnPrimary = E.rgb255 0xFF 0xFF 0xFF
    }


fontGreatVibes : E.Attribute msg
fontGreatVibes =
    EF.family [ EF.typeface "GreatVibes" ]


fontTypewriter : E.Attribute msg
fontTypewriter =
    EF.family [ EF.typeface "Typewriter" ]


type alias Model =
    { primaryDark : E.Color
    , secondaryDark : E.Color
    , textOnSecondary : E.Color
    , primaryLight : E.Color
    , primary : E.Color
    , secondary : E.Color
    , secondaryLight : E.Color
    , lightGray : E.Color
    , textOnPrimary : E.Color
    }


viewLayout : Model -> Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { backgroundColor = Nothing
                , borderColor = Just model.primaryDark
                , shadow = Nothing
                }
            ]
        }
        [ EBG.color model.secondaryDark
        , E.padding 22
        , EF.color model.textOnSecondary
        ]
        (E.column []
            [ buttonChangeColors model
            , viewTitle model
            , viewSubtitle model
            , dogImage
            , viewContent
            ]
        )


viewTitle : Model -> E.Element msg
viewTitle model =
    E.paragraph
        [ EF.bold
        , EF.color model.primary
        , fontGreatVibes
        , EF.size 52
        , ER.heading 1
        ]
        [ E.text "My Dog"
        ]


viewSubtitle : Model -> E.Element msg
viewSubtitle model =
    E.paragraph
        [ EF.color model.primaryLight
        , fontTypewriter
        , EF.size 16
        , E.paddingXY 0 10
        , ER.heading 2
        ]
        [ E.text "A web page for my dog"
        ]


dogImage : E.Element msg
dogImage =
    E.image
        [ E.width (E.maximum 300 E.fill)
        , E.centerX
        ]
        { src = "dog.png"
        , description = "A picture of my dog"
        }


buttonChangeColors : Model -> E.Element Msg
buttonChangeColors model =
    EI.button
        [ EBG.color model.primaryLight
        , EB.rounded 8
        , EF.color model.secondaryDark
        , E.alignRight
        , E.paddingEach { top = 12, right = 12, bottom = 12, left = 12 }
        , EF.size 16
        , EF.bold
        , E.mouseOver
            [ EB.color model.primary
            ]
        ]
        { onPress = Just MsgChangeColors
        , label = E.text "Change colors"
        }


text1 : String
text1 =
    "Chocolate cotton candy lemon drops cake lollipop icing lollipop. Bear claw dessert biscuit cake soufflé danish. Dragée sweet chocolate cake lollipop bonbon carrot cake cookie cupcake marshmallow. Cookie topping jelly brownie tiramisu chocolate cookie chocolate cake. Ice cream lemon drops jelly-o marshmallow bear claw shortbread ice cream ice cream. Danish marzipan chocolate cake sweet roll cake icing dessert. Jujubes cupcake dessert sesame snaps gummi bears dessert gummi bears. Chupa chups sesame snaps tart croissant liquorice sugar plum. Toffee lollipop lollipop chocolate cake pudding marzipan. Dragée marshmallow sweet wafer marzipan chocolate bar marzipan gummies. Dragée dessert toffee fruitcake shortbread bonbon chocolate bar donut. Lemon drops gummi bears sweet roll cheesecake toffee."


text2 : String
text2 =
    "Chupa chups cookie gummies biscuit croissant sweet toffee. Ice cream dragée fruitcake tiramisu apple pie toffee oat cake chocolate. Sweet roll caramels lemon drops bonbon jujubes sweet. Jelly-o powder dessert tart icing liquorice jelly beans lollipop. Bonbon lollipop tiramisu topping topping. Cake pudding carrot cake dessert carrot cake gingerbread sugar plum. Toffee croissant chupa chups sweet roll ice cream icing. Jelly apple pie muffin tootsie roll dragée gummies cupcake cookie cake. Tootsie roll jelly-o sesame snaps gingerbread marzipan tart icing. Wafer chupa chups candy canes fruitcake candy canes. Candy canes cookie lollipop icing candy canes cheesecake. Marshmallow jelly-o gummies gummies jelly. Cupcake jujubes caramels tootsie roll toffee pie. Cotton candy shortbread danish oat cake danish."


text3 : String
text3 =
    "Bonbon tart cotton candy brownie macaroon chupa chups muffin cheesecake. Soufflé apple pie chocolate cake jelly-o toffee cake soufflé lemon drops. Jelly-o candy canes cookie sweet toffee. Lemon drops sesame snaps toffee candy chocolate cake candy cake. Gummies dragée chupa chups soufflé tart cotton candy candy canes powder wafer. Sweet ice cream croissant apple pie pastry liquorice wafer pie. Sugar plum toffee soufflé chocolate cake soufflé chocolate cake donut jelly donut. Candy canes liquorice bear claw oat cake macaroon jelly. Sweet roll pudding muffin chocolate bar ice cream. Pudding bear claw wafer croissant cheesecake cheesecake. Liquorice sweet tootsie roll pastry toffee candy powder. Bonbon powder macaroon danish marshmallow biscuit chupa chups sweet roll. Marzipan icing danish chocolate cake powder gingerbread."


text4 : String
text4 =
    "Muffin biscuit carrot cake biscuit lollipop lemon drops bear claw pudding. Chocolate cake oat cake oat cake sugar plum dessert cake pastry. Candy tiramisu candy canes gingerbread wafer lollipop powder apple pie. Brownie toffee donut dragée marzipan sweet croissant cake icing. Candy canes gummi bears cake ice cream cake chocolate topping macaroon chocolate bar. Fruitcake jelly candy canes halvah bear claw jelly. Halvah jelly wafer jelly-o pie cotton candy marzipan pastry tootsie roll. Liquorice gummies chupa chups chocolate bar cheesecake cookie. Marshmallow dragée pudding lollipop pudding. Shortbread candy tart topping bear claw. Cake pudding gummies sweet roll powder. Gummies danish jelly beans marzipan pie fruitcake brownie donut. Carrot cake cotton candy shortbread dragée caramels cheesecake biscuit."


text5 =
    "Chocolate cupcake lollipop caramels biscuit halvah gummies powder cookie. Brownie jujubes sweet roll cheesecake tart jelly sweet. Jelly-o tootsie roll chocolate cake icing fruitcake sweet roll pudding gingerbread. Macaroon candy canes tootsie roll brownie dragée apple pie cake gummi bears. Cake bear claw bonbon carrot cake lemon drops. Liquorice brownie tart tiramisu sesame snaps. Ice cream soufflé chocolate cake bear claw apple pie. Candy fruitcake apple pie brownie tart sugar plum cake sweet roll. Gingerbread fruitcake cheesecake sweet roll jelly. Dessert tart topping muffin macaroon candy canes cheesecake soufflé fruitcake. Dessert candy canes marshmallow chocolate bar sweet candy canes chupa chups chocolate bar powder. Fruitcake gingerbread biscuit pastry liquorice. Gummi bears sesame snaps brownie cheesecake shortbread shortbread croissant."


paddingTop : Int -> E.Attribute msg
paddingTop size =
    E.paddingEach { top = size, right = 0, bottom = 0, left = 0 }


viewContent : E.Element msg
viewContent =
    E.column
        [ fontTypewriter
        , EF.size 16
        , paddingTop 20
        , ER.mainContent
        ]
        [ E.paragraph
            [ E.paddingXY 0 20
            ]
            [ E.text text1
            ]
        , E.paragraph
            [ E.paddingXY 0 20 ]
            [ E.text text2 ]
        , E.paragraph
            [ E.paddingXY 0 20 ]
            [ E.text text3 ]
        , E.paragraph
            [ E.paddingXY 0 20 ]
            [ E.text text4 ]
        , E.paragraph
            [ E.paddingXY 0 20 ]
            [ E.text text5 ]
        ]
