module Main exposing (Msg(..), init, main, update)

import Attr
import Auth exposing (Msg(..))
import Browser
import Browser.Events exposing (onKeyPress)
import Color exposing (blue, lightCharcoal)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Json.Decode
import Pages.Display as Display
import Pages.Registration as Registration
import Pages.SavedPlans as SavedPlans
import PlanParsers.Json exposing (..)
import Ports exposing (dumpModel)
import Time
import Types exposing (AppState, initAppState)


type Page
    = InputPage
    | DisplayPage Display.Model
    | LoginPage
    | SavedPlansPage SavedPlans.Model
    | RegistrationPage Registration.Model


type Msg
    = SubmitPlan
    | ChangePlanText String
    | ToggleMenu
    | CreatePlan
    | RequestLogin
    | Auth Auth.Msg
    | RequestSavedPlans Auth.SessionId
    | SavedPlansMsg SavedPlans.Msg
    | RequestLogout
    | DumpModel ()
    | NoOp
    | Register Registration.Msg
    | RequestRegistration
    | DisplayMsg Display.Msg


type alias Model =
    { appState : AppState
    , currPage : Page
    , selectedNode : Maybe Plan
    , savedPlans : List SavedPlan
    }


type alias Flags =
    { sessionId : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { appState = initAppState flags.sessionId
      , currPage = InputPage
      , selectedNode = Nothing
      , savedPlans = []
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS
-- Sub.batch allows us to combine multiple subscriptions into a single subscription
{-
   onKeyPress requires a JSON decoder that will extract the relevant information from the event value. The event value is a
   JSON representation of JavaScript KeyBoardEvent.
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dumpModel DumpModel
        , Time.every (100 * 1000) <| Auth << Auth.SendHeartBeat
        , onKeyPress <| keyDecoder model
        ]


keyDecoder : Model -> Json.Decode.Decoder Msg
keyDecoder model =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "altKey" Json.Decode.bool)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        |> Json.Decode.andThen
            (\altAndShiftFlags ->
                case altAndShiftFlags of
                    ( True, True ) ->
                        Json.Decode.field "code" Json.Decode.string
                            |> Json.Decode.map (keyToMsg model)

                    _ ->
                        Json.Decode.succeed NoOp
            )


keyToMsg : Model -> String -> Msg
keyToMsg model s =
    case ( s, model.appState.auth.sessionId ) of
        ( "s", Just sessionId ) ->
            RequestSavedPlans sessionId

        ( "n", _ ) ->
            CreatePlan

        _ ->
            NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ appState } as model) =
    case ( msg, model.currPage ) of
        ( ChangePlanText s, InputPage ) ->
            ( { model | appState = { appState | currPlanText = s } }, Cmd.none )

        ( SubmitPlan, InputPage ) ->
            ( { model | currPage = DisplayPage Display.init }, Cmd.none )

        ( DisplayMsg dispMsg, DisplayPage pageModel ) ->
            let
                newPageModel : Display.Model
                newPageModel =
                    Display.update dispMsg pageModel
            in
            ( { model | currPage = DisplayPage newPageModel }, Cmd.none )

        ( ToggleMenu, _ ) ->
            ( { model | appState = { appState | isMenuOpen = not appState.isMenuOpen } }, Cmd.none )

        ( CreatePlan, _ ) ->
            ( model, Cmd.none )

        -- Login page can be requested from any page via
        ( RequestLogin, _ ) ->
            ( { model | currPage = LoginPage }, Cmd.none )

        ( RequestSavedPlans sessionId, _ ) ->
            let
                ( pageModel, pageCmd ) =
                    SavedPlans.init appState.serverUrl sessionId
            in
            ( { model | currPage = SavedPlansPage pageModel }
            , Cmd.map SavedPlansMsg pageCmd
            )

        ( SavedPlansMsg pageMsg, SavedPlansPage pageModel ) ->
            let
                ( newPageModel, outMsg ) =
                    SavedPlans.update pageMsg pageModel

                newModel : Model
                newModel =
                    case outMsg of
                        SavedPlans.DisplayPlan planText ->
                            { model
                                | appState = { appState | currPlanText = planText }
                                , currPage = DisplayPage Display.init
                            }

                        _ ->
                            { model | currPage = SavedPlansPage newPageModel }
            in
            ( newModel, Cmd.none )

        ( RequestLogout, _ ) ->
            init { sessionId = Nothing }

        ( DumpModel (), _ ) ->
            ( Debug.log "model" model, Cmd.none )

        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( Auth authMsg, _ ) ->
            let
                ( authModel, authCmd ) =
                    Auth.update appState.serverUrl authMsg model.appState.auth

                currPage : Page
                currPage =
                    case authMsg of
                        Auth.FinishLogin (Ok _) ->
                            InputPage

                        _ ->
                            model.currPage
            in
            ( { model | appState = { appState | auth = authModel }, currPage = currPage }, Cmd.map Auth authCmd )

        ( Register regMsg, RegistrationPage pageModel ) ->
            let
                ( regModel, regCmd, pageMsg ) =
                    Registration.update regMsg model.appState pageModel

                newModel : Model
                newModel =
                    case pageMsg of
                        Registration.FinishSuccessfully id ->
                            let
                                auth =
                                    appState.auth
                            in
                            { appState =
                                { appState
                                    | auth = { auth | sessionId = Just id }
                                }
                            , currPage = InputPage
                            , selectedNode = model.selectedNode
                            , savedPlans = model.savedPlans
                            }

                        Registration.DoNothing ->
                            { model | currPage = RegistrationPage regModel }
            in
            ( newModel, Cmd.map Register regCmd )

        ( RequestRegistration, _ ) ->
            ( { model | currPage = RegistrationPage Registration.init }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content : Element Msg
        content =
            case model.currPage of
                InputPage ->
                    inputPage model.appState

                DisplayPage pageModel ->
                    Display.page model.appState pageModel
                        |> Element.map DisplayMsg

                LoginPage ->
                    loginPage model

                SavedPlansPage pageModel ->
                    SavedPlans.page pageModel |> Element.map SavedPlansMsg

                RegistrationPage pageModel ->
                    Registration.page pageModel
                        |> Element.map Register
    in
    { title = "VisExp"
    , body =
        [ layout [ inFront <| menuPanel model ] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


inputPage : AppState -> Element Msg
inputPage appState =
    column
        [ width (px 800)
        , spacingXY 0 10
        , centerX
        ]
        [ Input.multiline
            [ height (px 300)
            , Border.width 1
            , Border.rounded 3
            , Border.color lightCharcoal
            , padding 3
            ]
            { onChange = ChangePlanText
            , text = appState.currPlanText
            , placeholder = Nothing
            , label =
                Input.labelAbove [] <|
                    text "Paste the EXPLAIN output in JSON format:"
            , spellcheck = False
            }
        , Input.button
            (Attr.greenButton
                ++ [ width (px 200)
                   , height (px 40)
                   ]
            )
            { onPress = Just SubmitPlan
            , label = el [ centerX ] <| text "Go!"
            }
        ]


menuPanel : Model -> Element Msg
menuPanel model =
    let
        items : List (Element Msg)
        items =
            [ el [ pointer, onClick CreatePlan ] <| text "New Plan" ]
                ++ (case model.appState.auth.sessionId of
                        Just sessionId ->
                            [ el [ pointer, onClick <| RequestSavedPlans sessionId ] <| text "Saved plans"
                            , el [ pointer, onClick RequestLogout ] <| text "Logout"
                            ]

                        Nothing ->
                            [ el [ pointer, onClick RequestLogin ] <| text "Login"
                            , el [ pointer, onClick RequestRegistration ] <| text "Register"
                            ]
                   )

        panel : Element Msg
        panel =
            column
                [ Background.color Color.white
                , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
                , Border.color Color.grey
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 10
                    , color = Color.lightCharcoal
                    }
                , Font.bold
                , Font.color Color.darkCharcoal
                , Font.family [ Font.sansSerif ]
                , width <| fillPortion 1
                , height fill
                , paddingXY 20 20
                , spacingXY 0 20
                ]
                items

        overlay : Element Msg
        overlay =
            el [ width <| fillPortion 4, height fill, onClick ToggleMenu ] none
    in
    if model.appState.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 5, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ alignLeft ] <| text "VisExp"
        , Input.button (Attr.greyButton ++ [ padding 5, alignRight, width <| px 80 ])
            { onPress = Just ToggleMenu
            , label = el [ centerX ] <| text "Menu"
            }
        ]


loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ Input.username Attr.input
            { onChange = Auth << Auth.ChangeUserName
            , text = model.appState.auth.userName
            , label = Input.labelAbove [] <| text "User Name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = Auth << Auth.ChangePassword
            , text = model.appState.auth.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just <| Auth Auth.StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el Attr.error <| text model.appState.lastError
        ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
