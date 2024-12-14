module Pages.Registration exposing (Model, Msg(..), Platform(..), init, page, update)

import Attr
import Debug exposing (toString)
import Element exposing (Element, centerX, column, el, padding, paddingXY, px, spacing, spacingXY, text, width)
import Element.Input as Input
import Http
import Json.Decode
import Json.Encode
import Ports exposing (saveSessionId)
import Types exposing (AppState)


type Msg
    = ChangePassword String
    | ChangeRepeatPassword String
    | ChangeUserName String
    | FinishRegistration (Result Http.Error String)
    | SelectPlatform Platform
    | StartRegistration
    | ToggleAcceptTerms Bool


type Platform
    = Aws
    | Azure
    | Heroku
    | Selfhosted


type alias Model =
    { errors : List String
    , hasAcceptedTerms : Bool
    , password : String
    , platform : Maybe Platform
    , repeatPassword : String
    , userName : String
    }


init : Model
init =
    { hasAcceptedTerms = False
    , errors = []
    , password = ""
    , repeatPassword = ""
    , userName = ""
    , platform = Nothing
    }


type alias AppStateSubset a =
    { a | serverUrl : String, sessionId : Maybe String }


update : Msg -> AppState -> Model -> ( AppState, Model, Cmd Msg )
update msg ({ auth } as appState) model =
    case msg of
        ChangePassword p ->
            ( appState, { model | password = p }, Cmd.none )

        ChangeRepeatPassword p ->
            ( appState, { model | repeatPassword = p }, Cmd.none )

        ChangeUserName u ->
            ( appState, { model | userName = u }, Cmd.none )

        FinishRegistration (Ok sessionId) ->
            ( { appState | auth = { auth | sessionId = Just sessionId } }
            , model
            , saveSessionId <| Just sessionId
            )

        FinishRegistration (Err err) ->
            ( appState, { model | errors = [ toString err ] }, Cmd.none )

        SelectPlatform platform ->
            ( appState, { model | platform = Just platform }, Cmd.none )

        StartRegistration ->
            ( appState, model, register appState.serverUrl model.userName model.password )

        ToggleAcceptTerms val ->
            ( appState, { model | hasAcceptedTerms = val }, Cmd.none )


register : String -> String -> String -> Cmd Msg
register serverUrl userName password =
    let
        body : Http.Body
        body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "userName", Json.Encode.string userName )
                    , ( "password", Json.Encode.string password )
                    ]

        responseDecoder : Json.Decode.Decoder String
        responseDecoder =
            Json.Decode.field "sessionId" Json.Decode.string
    in
    Http.post
        { url = serverUrl ++ "register"
        , body = body
        , expect = Http.expectJson FinishRegistration responseDecoder
        }


page : Model -> Element Msg
page model =
    column
        [ paddingXY 0 20, spacingXY 0 10, width <| px 300, centerX ]
    <|
        [ Input.email Attr.input
            { onChange = ChangeUserName
            , text = model.userName
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Email: "
            }
        , Input.newPassword Attr.input
            { onChange = ChangePassword
            , text = model.password
            , placeholder = Nothing
            , show = False
            , label = Input.labelAbove [] <| text "Password: "
            }
        , Input.newPassword Attr.input
            { onChange = ChangeRepeatPassword
            , text = model.password
            , placeholder = Nothing
            , show = False
            , label = Input.labelAbove [] <| text "Repeat Password: "
            }
        , Input.radio
            [ padding 3, spacing 5 ]
            { onChange = SelectPlatform
            , selected = model.platform
            , label = Input.labelAbove [] <| text "Platform: "
            , options =
                [ Input.option Aws <| text "AWS"
                , Input.option Azure <| text "Azure"
                , Input.option Heroku <| text "Heroku"
                , Input.option Selfhosted <| text "Self-hosted"
                ]
            }
        , Input.checkbox
            [ padding 3 ]
            { onChange = ToggleAcceptTerms
            , checked = model.hasAcceptedTerms
            , label = Input.labelRight [] <| text "I accept the terms"
            , icon = Input.defaultCheckbox
            }
        , Input.button Attr.greenButton
            { onPress = Just StartRegistration
            , label = el [ centerX ] <| text "Register"
            }
        ]
            ++ List.map (text >> el Attr.error) model.errors
