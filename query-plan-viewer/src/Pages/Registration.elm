module Pages.Registration exposing (Model, Msg(..), PageMsg(..), Platform(..), init, page, update)

import Attr
import Debug exposing (toString)
import Element exposing (Element, centerX, column, el, padding, paddingXY, px, spacing, spacingXY, text, width)
import Element.Input as Input
import Http
import Json.Decode
import Json.Encode
import Ports exposing (saveSessionId)
import Validate exposing (..)


type PageMsg
    = DoNothing
    | FinishSuccessfully String


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


update : Msg -> { a | serverUrl : String } -> Model -> ( Model, Cmd Msg, PageMsg )
update msg { serverUrl } model =
    case msg of
        ChangePassword p ->
            ( { model | password = p }, Cmd.none, DoNothing )

        ChangeRepeatPassword p ->
            ( { model | repeatPassword = p }, Cmd.none, DoNothing )

        ChangeUserName u ->
            ( { model | userName = u }, Cmd.none, DoNothing )

        FinishRegistration (Ok sessionId) ->
            ( model
            , saveSessionId <| Just sessionId
            , FinishSuccessfully sessionId
            )

        FinishRegistration (Err err) ->
            ( { model | errors = [ toString err ] }, Cmd.none, DoNothing )

        SelectPlatform platform ->
            ( { model | platform = Just platform }, Cmd.none, DoNothing )

        StartRegistration ->
            case validate validator model of
                Ok validModel ->
                    ( fromValid validModel
                    , register serverUrl model.userName model.password
                    , DoNothing
                    )

                Err errors ->
                    ( { model | errors = errors }, Cmd.none, DoNothing )

        ToggleAcceptTerms val ->
            ( { model | hasAcceptedTerms = val }, Cmd.none, DoNothing )


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


validator : Validator String Model
validator =
    Validate.all
        [ Validate.ifInvalidEmail .userName
            (\_ -> "Please enter a valid email address.")
        , Validate.ifBlank .password "Password can't be blank."
        , Validate.fromErrors
            (\model ->
                if model.password == model.repeatPassword then
                    []

                else
                    [ "Passwords must match." ]
            )
        , Validate.fromErrors
            (\model ->
                if model.platform /= Nothing then
                    []

                else
                    [ "Pleas select a platform." ]
            )
        , Validate.ifFalse .hasAcceptedTerms "Please accept the terms."
        ]
