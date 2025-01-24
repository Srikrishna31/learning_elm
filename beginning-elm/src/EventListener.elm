module EventListener exposing (..)

import Browser
import Browser.Events exposing (onClick, onKeyPress)
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
    = CharacterKey Char
    | ControlKey String
    | MouseClick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CharacterKey 'i' ->
            ( model + 1, Cmd.none )

        CharacterKey 'd' ->
            ( model - 1, Cmd.none )

        MouseClick ->
            ( model + 5, Cmd.none )

        _ ->
            ( model, Cmd.none )



{-
   When we have more than one subscription, we must batch them with Sub.batch:

       Sub.batch : List (Sub msg) -> Sub msg
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyPress keyDecoder
        , onClick <| Decode.succeed MouseClick
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey <| Decode.field "key" Decode.string



{-
   # String.uncons
   The String.uncons function splits a non-empty string into its head and tail.

       uncons: String -> Maybe (Char, String)
-}


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
