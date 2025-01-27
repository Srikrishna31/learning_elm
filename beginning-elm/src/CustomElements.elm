module CustomElements exposing (CropData)

import Asset
import Browser
import Html exposing (Attribute, Html, div, h2, strong, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (requiredAt)



{-
   # WebComponents
    Web Components are a set of APIs provided by the Web Platform for building reusable custom elements which contain all
    the necessary HTML, CSS and JavaScript code. These custom elements can be used inside any framework-including Elm-that
    works with HTML and JavaScript.

   # Installing Custom Elements
    We can install custom elements from webcomponents.org by using npm. The elm.json file is used to keep track of which
    Elm packages the project depends on. Similarly, package.json keeps track of all npm packages used the project. Create
    package.json by running following command:
        npm init -y

    Now run the following command in terminal to install the image-crop-element custom element

        npm install @github/image-crop-element
-}


type alias CropData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }



{-
   # Defining Custom Element Nodes
    In Elm, a tag is nothing but a simple wrapper to the Elm.Kernel.VirtualDom.node function. To use custom elements in
    Elm app, we need to use the Html.node function.
-}


imageCrop : List (Attribute a) -> List (Html a) -> Html a
imageCrop =
    Html.node "image-crop"


view : CropData -> Html Msg
view cropData =
    div [ class "center" ]
        [ h2 [] [ text "Image Crop" ]
        , imageCrop
            [ Asset.src Asset.waterfall
            , class "wrapper"
            ]
            []
        ]


type Msg
    = NoOp


update : Msg -> CropData -> ( CropData, Cmd Msg )
update msg cropData =
    case msg of
        NoOp ->
            ( cropData, Cmd.none )


initialModel : CropData
initialModel =
    { x = 297
    , y = 0
    , width = 906
    , height = 906
    }


init : () -> ( CropData, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


main : Program () CropData Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
