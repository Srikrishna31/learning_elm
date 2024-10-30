module Main exposing (main)

import Browser
import Html exposing (Html, button, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


initModel : Model
initModel =
    { message = "Welcome"
    , firstname = Nothing
    , age = Nothing
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }


type Msg
    = MsgSurprise
    | MsgReset
    | MsgNewName String
    | MsgNewAgeAsString String


type alias Model =
    { message : String
    , firstname : Maybe String
    , age : Maybe Int
    }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewMessage model.message
        , viewFirstnameInput model.firstname
        , viewAgeInput model.age
        , viewSurpriseButton
        , viewResetButton
        , viewLength model.firstname
        ]


viewMessage : String -> Html.Html Msg
viewMessage message =
    text message


viewFirstnameInput : Maybe String -> Html.Html Msg
viewFirstnameInput firstname =
    Html.input [ onInput MsgNewName, value (Maybe.withDefault "" firstname) ] []


viewAgeInput : Maybe Int -> Html.Html Msg
viewAgeInput age =
    Html.input [ onInput MsgNewAgeAsString, value (String.fromInt (Maybe.withDefault 0 age)) ] []


viewSurpriseButton : Html.Html Msg
viewSurpriseButton =
    button [ onClick MsgSurprise ] [ text "Surprise" ]


viewResetButton : Html Msg
viewResetButton =
    button [ onClick MsgReset ] [ text "Reset" ]


viewLength : Maybe String -> Html.Html Msg
viewLength firstname =
    Html.text (String.fromInt (String.length (Maybe.withDefault "" firstname)))


update : Msg -> Model -> Model
update msg model =
    case msg of
        MsgSurprise ->
            { model
                | message = "Happy Birthday !!" ++ Maybe.withDefault "" model.firstname ++ String.fromInt (Maybe.withDefault 0 model.age) ++ " years !!"
            }

        MsgReset ->
            initModel

        MsgNewName newName ->
            if String.trim newName == "" then
                { model
                    | firstname = Nothing
                }

            else
                { model
                    | firstname = Maybe.Just newName
                }

        MsgNewAgeAsString newAge ->
            case String.toInt newAge of
                Just anInt ->
                    { model
                        | age = Just anInt
                    }

                Nothing ->
                    { model
                        | message = "The age is wrong"
                        , age = Nothing
                    }
