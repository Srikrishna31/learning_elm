module Main exposing (main)

import Browser exposing (Document)
import Html exposing (text)


main : Program () Model msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Document msg
view model =
    { title = model.title
    , body = [ Html.text "Hello Document" ]
    }


type alias Model =
    { title : String
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { title = "My title"
    }


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
