module Main exposing (main)

import Browser
import Html
import Http
import Json.Decode


main : Program () Model Msg
main =
    Browser.element
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( { title = "Testing" }, getTitle )


view : Model -> Html.Html msg
view model =
    Html.text model.title


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MsgGotTitle result ->
            case result of
                Ok data ->
                    ( { model | title = data }, Cmd.none )

                Err _ ->
                    ( { model | title = "Error" }, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


type alias Model =
    { title : String
    }


type Msg
    = MsgGotTitle (Result Http.Error String)


getTitle : Cmd Msg
getTitle =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts/2"
        , expect = Http.expectJson MsgGotTitle dataTitleDecoder
        }


dataTitleDecoder : Json.Decode.Decoder String
dataTitleDecoder =
    Json.Decode.field "title" Json.Decode.string
