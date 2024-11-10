module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation
import Html
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


type alias Model =
    { title : String
    }


type Msg
    = MsgDummy


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ url navigationKey =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { title = "Hello Navigation"
    }


view : Model -> Document msg
view _ =
    { title = "Test"
    , body = [ viewContent ]
    }


viewContent : Html.Html msg
viewContent =
    Html.text "Navigation"


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MsgDummy ->
            ( model, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    MsgDummy


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    MsgDummy
