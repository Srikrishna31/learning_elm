module Main exposing (main)

import Browser
import Element
import Html exposing (text)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Size =
    { w : Int
    , h : Int
    }


type alias Model =
    { windowSize : Size
    }


type Msg
    = MsgDummy


init : () -> ( Model, Cmd.Cmd Msg )
init flags =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { windowSize =
        { w = 1024
        , h = 768
        }
    }


view : Model -> Html.Html Msg
view model =
    Element.layout []
        (Element.text (String.fromInt model.windowSize.w ++ "x" ++ String.fromInt model.windowSize.h))


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
