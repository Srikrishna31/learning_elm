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
            , viewContent
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
    Element.image
        [ Element.width (Element.maximum 300 Element.fill)
        , Element.centerX
        ]
        { src = "dog.png"
        , description = "A picture of my dog"
        }


text1 =
    "Chocolate cotton candy lemon drops cake lollipop icing lollipop. Bear claw dessert biscuit cake soufflé danish. Dragée sweet chocolate cake lollipop bonbon carrot cake cookie cupcake marshmallow. Cookie topping jelly brownie tiramisu chocolate cookie chocolate cake. Ice cream lemon drops jelly-o marshmallow bear claw shortbread ice cream ice cream. Danish marzipan chocolate cake sweet roll cake icing dessert. Jujubes cupcake dessert sesame snaps gummi bears dessert gummi bears. Chupa chups sesame snaps tart croissant liquorice sugar plum. Toffee lollipop lollipop chocolate cake pudding marzipan. Dragée marshmallow sweet wafer marzipan chocolate bar marzipan gummies. Dragée dessert toffee fruitcake shortbread bonbon chocolate bar donut. Lemon drops gummi bears sweet roll cheesecake toffee."


text2 =
    "Chupa chups cookie gummies biscuit croissant sweet toffee. Ice cream dragée fruitcake tiramisu apple pie toffee oat cake chocolate. Sweet roll caramels lemon drops bonbon jujubes sweet. Jelly-o powder dessert tart icing liquorice jelly beans lollipop. Bonbon lollipop tiramisu topping topping. Cake pudding carrot cake dessert carrot cake gingerbread sugar plum. Toffee croissant chupa chups sweet roll ice cream icing. Jelly apple pie muffin tootsie roll dragée gummies cupcake cookie cake. Tootsie roll jelly-o sesame snaps gingerbread marzipan tart icing. Wafer chupa chups candy canes fruitcake candy canes. Candy canes cookie lollipop icing candy canes cheesecake. Marshmallow jelly-o gummies gummies jelly. Cupcake jujubes caramels tootsie roll toffee pie. Cotton candy shortbread danish oat cake danish."


text3 =
    "Bonbon tart cotton candy brownie macaroon chupa chups muffin cheesecake. Soufflé apple pie chocolate cake jelly-o toffee cake soufflé lemon drops. Jelly-o candy canes cookie sweet toffee. Lemon drops sesame snaps toffee candy chocolate cake candy cake. Gummies dragée chupa chups soufflé tart cotton candy candy canes powder wafer. Sweet ice cream croissant apple pie pastry liquorice wafer pie. Sugar plum toffee soufflé chocolate cake soufflé chocolate cake donut jelly donut. Candy canes liquorice bear claw oat cake macaroon jelly. Sweet roll pudding muffin chocolate bar ice cream. Pudding bear claw wafer croissant cheesecake cheesecake. Liquorice sweet tootsie roll pastry toffee candy powder. Bonbon powder macaroon danish marshmallow biscuit chupa chups sweet roll. Marzipan icing danish chocolate cake powder gingerbread."


text4 =
    "Muffin biscuit carrot cake biscuit lollipop lemon drops bear claw pudding. Chocolate cake oat cake oat cake sugar plum dessert cake pastry. Candy tiramisu candy canes gingerbread wafer lollipop powder apple pie. Brownie toffee donut dragée marzipan sweet croissant cake icing. Candy canes gummi bears cake ice cream cake chocolate topping macaroon chocolate bar. Fruitcake jelly candy canes halvah bear claw jelly. Halvah jelly wafer jelly-o pie cotton candy marzipan pastry tootsie roll. Liquorice gummies chupa chups chocolate bar cheesecake cookie. Marshmallow dragée pudding lollipop pudding. Shortbread candy tart topping bear claw. Cake pudding gummies sweet roll powder. Gummies danish jelly beans marzipan pie fruitcake brownie donut. Carrot cake cotton candy shortbread dragée caramels cheesecake biscuit."


text5 =
    "Chocolate cupcake lollipop caramels biscuit halvah gummies powder cookie. Brownie jujubes sweet roll cheesecake tart jelly sweet. Jelly-o tootsie roll chocolate cake icing fruitcake sweet roll pudding gingerbread. Macaroon candy canes tootsie roll brownie dragée apple pie cake gummi bears. Cake bear claw bonbon carrot cake lemon drops. Liquorice brownie tart tiramisu sesame snaps. Ice cream soufflé chocolate cake bear claw apple pie. Candy fruitcake apple pie brownie tart sugar plum cake sweet roll. Gingerbread fruitcake cheesecake sweet roll jelly. Dessert tart topping muffin macaroon candy canes cheesecake soufflé fruitcake. Dessert candy canes marshmallow chocolate bar sweet candy canes chupa chups chocolate bar powder. Fruitcake gingerbread biscuit pastry liquorice. Gummi bears sesame snaps brownie cheesecake shortbread shortbread croissant."


paddingTop size =
    Element.paddingEach { top = size, right = 0, bottom = 0, left = 0 }


viewContent =
    Element.column
        [ fontTypewriter
        , Element.Font.size 16
        , paddingTop 20
        ]
        [ Element.paragraph
            [ Element.paddingXY 0 20
            ]
            [ Element.text text1
            ]
        , Element.paragraph
            [ Element.paddingXY 0 20 ]
            [ Element.text text2 ]
        , Element.paragraph
            [ Element.paddingXY 0 20 ]
            [ Element.text text3 ]
        , Element.paragraph
            [ Element.paddingXY 0 20 ]
            [ Element.text text4 ]
        , Element.paragraph
            [ Element.paddingXY 0 20 ]
            [ Element.text text5 ]
        ]
