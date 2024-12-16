module Auth exposing (Flags, Model, Msg(..), SessionId, init, update)

import Http
import Json.Decode
import Json.Encode
import Ports exposing (..)
import Time
import Utils exposing (httpErrorString)


type alias Flags =
    Maybe String


type alias SessionId =
    String


type Msg
    = NoOp
    | ChangePassword String
    | ChangeUserName String
    | FinishLogin (Result Http.Error String)
    | SendHeartBeat Time.Posix
    | StartLogin


type alias Model =
    { lastError : String
    , password : String
    , sessionId : Flags
    , userName : String
    }


init : Flags -> Model
init sessionId =
    { lastError = ""
    , password = ""
    , sessionId = sessionId
    , userName = ""
    }



{-
   Commands
   Commands provide a way to carry out asynchronous operations with side effects. When you issue a command by returning it
   from the update function, it causes the Elm runtime to kick off the corresponding operation and subsequently return the
   result back to your program via a message.

   As normal functions in Elm are pure, there's no way to express something like making a call to a server or generating a
   random number in a regular function, because every call to such a function might return a different value. Instead, anything
   that has side effects is done via commands.

   The update function returns a command in a tuple with the changed model.

   Server requests are made by sending particular commands to the Elm runtime. In order to make requests, we need to use the
   elm/http package from the Elm core library.
   Http.post returns a command that will generate a message once the request is complete. The http package provides functions
   such as get, post and a more general-purpose request to generate commands that trigger HTTP requests.
   The expect field in the argument determines how the response will be handled, and is a value of type Expect msg. The http
   package provides several functions to construct these values: expectString, expectJson, expectBytes and expectWhatever
   (which ignores the response):

   expectJson: (Result Error a -> msg) -> Decoder a -> Expect msg
-}


login : String -> String -> String -> Cmd Msg
login serverUrl userName password =
    let
        body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "userName", Json.Encode.string userName )
                    , ( "password", Json.Encode.string password )
                    ]

        responseDecoder =
            Json.Decode.field "sessionId" Json.Decode.string
    in
    Http.post
        { url = serverUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishLogin responseDecoder
        }


sendHeartBeat : String -> Maybe SessionId -> Cmd Msg
sendHeartBeat serverUrl sessionId =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "heartbeat"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever <| always NoOp
        }


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update serverUrl msg model =
    case msg of
        ChangePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        ChangeUserName newUser ->
            ( { model | userName = newUser }, Cmd.none )

        StartLogin ->
            ( model, login serverUrl model.userName model.password )

        FinishLogin (Ok sessionId) ->
            ( { model | sessionId = Just sessionId, userName = "", password = "" }
            , saveSessionId <| Just sessionId
            )

        FinishLogin (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        SendHeartBeat posix ->
            ( model, sendHeartBeat serverUrl model.sessionId )
