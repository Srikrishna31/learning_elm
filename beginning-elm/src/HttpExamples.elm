module HttpExamples exposing (..)

import Browser
import Html exposing (Html, button, div, h3, li, text, ul)
import Html.Events exposing (onClick)
import Http


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
    let
        errorHeading =
            "Couldn't fetch nicknames at this time"
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text <| "Error" ++ errorMessage
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
    | DataReceived (Result Http.Error String)


url : String
url =
    "http://localhost:5016/old-school.txt"



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
        , expect = Http.expectString DataReceived
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

        DataReceived (Ok nickNamesStr) ->
            let
                nickNames : List String
                nickNames =
                    String.split "," nickNamesStr
            in
            ( { model | names = nickNames }, Cmd.none )

        DataReceived (Err error) ->
            ( { model | errorMessage = Just <| buildErrorMessage error }, Cmd.none )


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