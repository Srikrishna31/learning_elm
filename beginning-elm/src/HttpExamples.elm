module HttpExamples exposing (..)

import Browser
import Html exposing (Html, button, div, h3, li, text, ul)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, Error(..), decodeString, field, list, string, succeed)


type alias Model =
    { names : List String
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewNickNamesOrError model
        ]


viewNickNamesOrError : Model -> Html Msg
viewNickNamesOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewNickNames model.names


viewError : String -> Html Msg
viewError errorMessage =
    div []
        [ h3 [] [ text "Couldn't fetch nicknames at this time" ]
        , text errorMessage
        ]


viewNickNames : List String -> Html Msg
viewNickNames nicknames =
    div []
        [ h3 [] [ text "Old School Main Characters" ]
        , ul [] <| List.map viewNickName nicknames
        ]


viewNickName : String -> Html Msg
viewNickName nickname =
    li [] [ text nickname ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List String))


url : String
url =
    "http://localhost:5019/names/1"



{-
   Here is what the Http.get function's type signature looks like:

   get: {url: String, expect: Expect msg} -> Cmd msg

   It takes a record with two fields and returns a command. The url field holds the location of the server resource. The
   expect field specifies the format we expect from the server.
-}


getNickNames : Cmd Msg
getNickNames =
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived nickNamesDecoder
        }



{-
   update handles the `SendHttpRequest` message by returning the original model and a command for fetching nicknames
   from the local HTTP server created earlier.
   For security reasons, most modern browsers restrict cross-origin HTTP requests initiated through an ajax call which
   uses the `XMLHttpRequest` JavaScript object behind the scenes.
   The Elm runtime uses ajax to send all HTTP requests under the hood.

   `Access-Control-Allow-Origin` is one of the headers included in a response sent by the server. It indicates which domains
    are allowed to use the response. For example, if the server returns Access-Control-Allow-Origin:*, the response can
    be used by any domain. But if the server wants only certain domains to have access then it'll return a domain name(s)
    instead of *, e.g: Access-Control-Allow-Origin: http://localhost:8000

    Note: Most browsers cache CORS policies for sometime in non-private mode. That's why we need to open HttpExamples.elm
    in private mode. Otherwise we keep getting the same CORS error, even after refreshing the page.
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getNickNames )

        DataReceived (Ok nickNames) ->
            ( { model | names = nickNames }, Cmd.none )

        DataReceived (Err error) ->
            ( { model | errorMessage = Just <| buildErrorMessage error }, Cmd.none )


handleJsonError : Json.Decode.Error -> Maybe String
handleJsonError error =
    case error of
        Field string _ ->
            Just string

        --
        --Index int error ->
        --
        --
        --OneOf errors ->
        Failure errorMessage _ ->
            Just errorMessage

        _ ->
            Just "Error: Invalid JSON"


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later"

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus int ->
            "Request failed with status code" ++ String.fromInt int

        Http.BadBody message ->
            message


init : () -> ( Model, Cmd msg )
init _ =
    ( { names = [], errorMessage = Nothing }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



{-

   decodeString: Decoder a -> String -> Result Error a

   decodeString first parses the raw json and then uses a given decoder to translate that JSON to respective Elm values.

   The Error type is defined in the Json.Decode module like this:

        type Error
            = Field String Error
            | Index Int Error
            | OneOf (List Error)
            | Failure String Value

    It's a recursive type.
-}


nickNamesDecoder : Decoder (List String)
nickNamesDecoder =
    field "nicknames" <| list string
