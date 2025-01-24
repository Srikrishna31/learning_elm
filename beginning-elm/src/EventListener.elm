module EventListener exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, div, text)
import Json.Decode as Decode


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text <| String.fromInt model ]


type Msg
    = KeyPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed ->
            ( model + 1, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress <| Decode.succeed KeyPressed


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
