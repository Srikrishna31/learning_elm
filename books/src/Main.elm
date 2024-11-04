module Main exposing (main)

import Browser
import Html


type alias Model =
    { searchText : String
    , results : List String
    }


type Msg
    = MsgSearch
    | MsgGotResults
    | MsgInputTextField


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { searchText = ""
    , results = []
    }


view : Model -> Html.Html Msg
view _ =
    Html.text "Books"


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
