module Main exposing (main)

import Browser exposing (Document)
import Html exposing (text)
import Html.Events exposing (onClick)


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Document Msg
view model =
    { title = model.title
    , body = [ Html.button [ onClick MsgIncreaseCounter ] [ Html.text ("Counter " ++ String.fromInt model.counter) ] ]
    }


type Msg
    = MsgIncreaseCounter


type alias Model =
    { title : String
    , counter : Int
    }


init : Int -> ( Model, Cmd msg )
init start =
    ( { initModel | counter = start }, Cmd.none )


initModel : Model
initModel =
    { title = "My title"
    , counter = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgIncreaseCounter ->
            let
                newCounter =
                    model.counter + 1

                newTitle =
                    "My Title" ++ String.fromInt newCounter

                newModel =
                    { model | counter = newCounter, title = newTitle }
            in
            ( newModel, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
