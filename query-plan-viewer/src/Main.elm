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
import PlanParsers.Json exposing (..)
import PlanTree
import Ports exposing (dumpModel, saveSessionId)
import Time
import Utils exposing (httpErrorString)


type Page
    = InputPage
    | DisplayPage
    | LoginPage
    | SavedPlansPage


type Msg
    = SubmitPlan
    | ChangePlanText String
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan
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


type alias Model =
    { auth : Auth.Model
    , currPage : Page
    , currPlanText : String
    , selectedNode : Maybe Plan
    , isMenuOpen : Bool
    , savedPlans : List SavedPlan
    , lastError : String
    }


type alias Flags =
    { sessionId : Maybe String
    }


serverUrl : String
serverUrl =
    ""


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { auth = Auth.init flags.sessionId
      , currPage = InputPage
      , currPlanText =
            """
      {                                                           
        "Plan": {                                                 
          "Node Type": "Hash Join",                               
          "Parallel Aware": false,                                
          "Join Type": "Inner",                                   
          "Startup Cost": 1.04,                                   
          "Total Cost": 2.11,                                     
          "Plan Rows": 2,                                         
          "Plan Width": 176,                                      
          "Actual Startup Time": 0.061,                           
          "Actual Total Time": 0.066,                             
          "Actual Rows": 8,                                       
          "Actual Loops": 1,                                      
          "Hash Cond": "(zones.project_id = projects.project_id)",
          "Plans": [                                              
              {                                                     
              "Node Type": "Seq Scan",                            
              "Parent Relationship": "Outer",                     
              "Parallel Aware": false,                            
              "Relation Name": "zones",                           
              "Alias": "zones",                                   
              "Startup Cost": 0.00,                               
              "Total Cost": 1.03,                                 
              "Plan Rows": 3,                                     
              "Plan Width": 112,                                  
              "Actual Startup Time": 0.013,                       
              "Actual Total Time": 0.015,                         
              "Actual Rows": 4,                                   
              "Actual Loops": 1                                   
              },                                                    
              {                                                     
              "Node Type": "Hash",                                
              "Parent Relationship": "Inner",                     
              "Parallel Aware": false,                            
              "Startup Cost": 1.02,                               
              "Total Cost": 1.02,                                 
              "Plan Rows": 2,                                     
              "Plan Width": 72,                                   
              "Actual Startup Time": 0.013,                       
              "Actual Total Time": 0.013,                         
              "Actual Rows": 4,                                   
              "Actual Loops": 1,                                  
              "Hash Buckets": 1024,                               
              "Original Hash Buckets": 1024,                      
              "Hash Batches": 1,                                  
              "Original Hash Batches": 1,                         
              "Peak Memory Usage": 9,                             
              "Plans": [                                          
                  {                                                 
                  "Node Type": "Seq Scan",                        
                  "Parent Relationship": "Outer",                 
                  "Parallel Aware": false,                        
                  "Relation Name": "projects",                    
                  "Alias": "projects",                            
                  "Startup Cost": 0.00,                           
                  "Total Cost": 1.02,                             
                  "Plan Rows": 2,                                 
                  "Plan Width": 72,                               
                  "Actual Startup Time": 0.004,                   
                  "Actual Total Time": 0.005,                     
                  "Actual Rows": 4,                               
                  "Actual Loops": 1                               
                  }                                                 
              ]                                                   
              }                                                     
          ]                                                       
        },                                                        
        "Planning Time": 0.174,                                   
        "Triggers": [                                             
        ],                                                        
        "Execution Time": 0.169                                   
      }                                                           

      """
      , selectedNode = Nothing
      , isMenuOpen = False
      , savedPlans = []
      , lastError = ""
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
    case ( s, model.auth.sessionId ) of
        ( "s", Just _ ) ->
            RequestSavedPlans

        ( "n", _ ) ->
            CreatePlan

        _ ->
            NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePlanText s ->
            ( { model | currPlanText = s }, Cmd.none )

        SubmitPlan ->
            ( { model | currPage = DisplayPage }, Cmd.none )

        MouseEnteredPlanNode plan ->
            ( { model | selectedNode = Just plan }, Cmd.none )

        MouseLeftPlanNode _ ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        CreatePlan ->
            ( model, Cmd.none )

        RequestLogin ->
            ( { model | currPage = LoginPage }, Cmd.none )

        RequestSavedPlans ->
            ( { model | currPage = SavedPlansPage }, getSavedPlans model.auth.sessionId )

        FinishSavedPlans (Ok savedPlans) ->
            ( { model | savedPlans = savedPlans }, Cmd.none )

        FinishSavedPlans (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        ShowPlan planText ->
            ( { model | currPlanText = planText, currPage = DisplayPage }, Cmd.none )

        RequestLogout ->
            init { sessionId = Nothing }

        DumpModel () ->
            ( Debug.log "model" model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Auth authMsg ->
            let
                ( authModel, authCmd ) =
                    Auth.update serverUrl authMsg model.auth

                currPage : Page
                currPage =
                    case authMsg of
                        Auth.FinishLogin (Ok _) ->
                            InputPage

                        _ ->
                            model.currPage
            in
            ( { model | auth = authModel, currPage = currPage }, Cmd.map Auth authCmd )



{-
   Even though it's a GET request, we have to use Http.request rather than Http.get because this request requires a custom
   HTTP header with the session ID. The request function takes a record with various parameters.
-}


getSavedPlans : Maybe String -> Cmd Msg
getSavedPlans sessionId =
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

                DisplayPage ->
                    displayPage model

                LoginPage ->
                    loginPage model

                SavedPlansPage ->
                    savedPlansPage model
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
            , text = model.currPlanText
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
                ++ (case model.auth.sessionId of
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
    if model.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


displayPage : Model -> Element Msg
displayPage model =
    let
        planTreeConfig : PlanTree.Config Msg
        planTreeConfig =
            { onMouseEnteredNode = MouseEnteredPlanNode
            , onMouseLeftNode = MouseLeftPlanNode
            }
    in
    case Json.Decode.decodeString decodePlanJson model.currPlanText of
        Ok planJson ->
            PlanTree.render planTreeConfig planJson model.selectedNode

        Err err ->
            el [] <| text <| Json.Decode.errorToString err


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
            , text = model.auth.userName
            , label = Input.labelAbove [] <| text "User Name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = Auth << Auth.ChangePassword
            , text = model.auth.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just <| Auth Auth.StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el Attr.error <| text model.lastError
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
