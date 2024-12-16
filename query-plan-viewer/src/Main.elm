module Main exposing (main)

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
import Http exposing (Body)
import Json.Decode
import Json.Encode
import Pages.Display as Display
import Pages.Registration as Registration
import PlanParsers.Json exposing (..)
import PlanTree
import Ports exposing (dumpModel, saveSessionId)
import Time
import Types exposing (AppState, initAppState)
import Utils exposing (httpErrorString)


type Page
    = InputPage
    | DisplayPage Display.Model
    | LoginPage
    | SavedPlansPage
    | RegistrationPage Registration.Model


type Msg
    = SubmitPlan
    | ChangePlanText String
    | ToggleMenu
    | CreatePlan
    | RequestLogin
    | Auth Auth.Msg
    | RequestSavedPlans
    | FinishSavedPlans (Result Http.Error (List SavedPlan))
    | ShowPlan String
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
        ( "s", Just _ ) ->
            RequestSavedPlans

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

        ( RequestSavedPlans, _ ) ->
            ( { model | currPage = SavedPlansPage }, getSavedPlans model.appState.serverUrl model.appState.auth.sessionId )

        ( FinishSavedPlans (Ok savedPlans), _ ) ->
            ( { model | savedPlans = savedPlans }, Cmd.none )

        ( FinishSavedPlans (Err error), _ ) ->
            ( { model | appState = { appState | lastError = httpErrorString error } }, Cmd.none )

        --( ShowPlan planText, _ ) ->
        --    ( { model | appState = { appState | currPlanText = planText }, currPage = DisplayPage }, Cmd.none )
        --
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



{-
   Even though it's a GET request, we have to use Http.request rather than Http.get because this request requires a custom
   HTTP header with the session ID. The request function takes a record with various parameters.
-}


getSavedPlans : String -> Maybe String -> Cmd Msg
getSavedPlans serverUrl sessionId =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "plans"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson FinishSavedPlans decodeSavedPlans
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content : Element Msg
        content =
            case model.currPage of
                InputPage ->
                    inputPage model

                DisplayPage pageModel ->
                    Display.page model.appState pageModel
                        |> Element.map DisplayMsg

                LoginPage ->
                    loginPage model

                SavedPlansPage ->
                    savedPlansPage model

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


inputPage : Model -> Element Msg
inputPage model =
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
            , text = model.appState.currPlanText
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
                        Just _ ->
                            [ el [ pointer, onClick RequestSavedPlans ] <| text "Saved plans"
                            , el [ pointer, onClick RequestLogout ] <| text "Logout"
                            ]

                        Nothing ->
                            [ el [ pointer, onClick RequestLogin ] <| text "Login" ]
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


savedPlansPage : Model -> Element Msg
savedPlansPage model =
    let
        annotateVersion name planVersion =
            { version = planVersion.version
            , planText = planVersion.planText
            , createdAt = planVersion.createdAt
            , name = name
            }

        annotateVersions savedPlan =
            List.map (annotateVersion savedPlan.name) savedPlan.versions

        tableAttrs =
            [ width (px 800)
            , paddingEach { top = 10, bottom = 50, left = 10, right = 10 }
            , spacingXY 10 10
            , centerX
            ]

        headerAttrs =
            [ Font.bold
            , Background.color Color.lightGrey
            , Border.color Color.darkCharcoal
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , centerX
            ]
    in
    table tableAttrs
        { data = List.concatMap annotateVersions model.savedPlans
        , columns =
            [ { header = el headerAttrs <| text "Plan name"
              , width = fill
              , view =
                    \plan ->
                        el
                            [ Font.underline
                            , mouseOver [ Font.color lightCharcoal ]
                            , onClick <| ShowPlan plan.planText
                            ]
                        <|
                            text plan.name
              }
            , { header = el headerAttrs <| text "Creation time"
              , width = fill
              , view = .createdAt >> text
              }
            , { header = el headerAttrs <| text "Version"
              , width = fill
              , view = .version >> String.fromInt >> text
              }
            ]
        }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
