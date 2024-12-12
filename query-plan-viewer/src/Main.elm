module Main exposing (main)

import Attr
import Browser
import Browser.Events exposing (onKeyPress)
import Color exposing (blue, darkGreen, green, grey, lightBlue, lightCharcoal, lightGrey, lightYellow, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes exposing (id)
import Http exposing (Body)
import Json.Decode
import Json.Encode
import PlanParsers.Json as P exposing (..)
import Ports exposing (dumpModel, saveSessionId)
import Time


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
    | ChangePassword String
    | ChangeUserName String
    | StartLogin
    | FinishLogin (Result Http.Error String)
    | RequestSavedPlans
    | FinishSavedPlans (Result Http.Error (List SavedPlan))
    | ShowPlan String
    | RequestLogout
    | DumpModel ()
    | SendHeartBeat Time.Posix
    | NoOp


type alias Model =
    { currPage : Page
    , currPlanText : String
    , selectedNode : Maybe Plan
    , isMenuOpen : Bool
    , userName : String
    , password : String
    , lastError : String
    , sessionId : Maybe String
    , savedPlans : List SavedPlan
    }


type alias Flags =
    { sessionId : Maybe String
    }


serverUrl : String
serverUrl =
    ""


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currPage = InputPage
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
      , userName = ""
      , password = ""
      , lastError = ""
      , sessionId = flags.sessionId
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
        , Time.every (100 * 1000) SendHeartBeat
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
    case ( s, model.sessionId ) of
        ( "s", Just id ) ->
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

        MouseLeftPlanNode commonFields ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        CreatePlan ->
            ( model, Cmd.none )

        RequestLogin ->
            ( { model | currPage = LoginPage, password = "", userName = "" }, Cmd.none )

        ChangePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        ChangeUserName newUser ->
            ( { model | userName = newUser }, Cmd.none )

        StartLogin ->
            ( model, login model.userName model.password )

        FinishLogin (Ok sessionId) ->
            ( { model | sessionId = Just sessionId, currPage = InputPage }
            , saveSessionId <| Just sessionId
            )

        FinishLogin (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        RequestSavedPlans ->
            ( { model | currPage = SavedPlansPage }, getSavedPlans model.sessionId )

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

        SendHeartBeat _ ->
            ( model, sendHeartBeat model.sessionId )

        NoOp ->
            ( model, Cmd.none )


sendHeartBeat : Maybe String -> Cmd Msg
sendHeartBeat sessionId =
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


login : String -> String -> Cmd Msg
login userName password =
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


httpErrorString : Http.Error -> String
httpErrorString err =
    case err of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.Timeout ->
            "Request Timed out"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
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
                ++ (case model.sessionId of
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
        tree =
            case Json.Decode.decodeString decodePlanJson model.currPlanText of
                Ok planJson ->
                    planNodeTree planJson.plan

                Err err ->
                    [ text <| Json.Decode.errorToString err ]

        details =
            case model.selectedNode of
                Nothing ->
                    [ text "" ]

                Just plan ->
                    detailPanelContent plan
    in
    row [ width fill, paddingEach { top = 20, left = 0, right = 0, bottom = 0 } ]
        [ column [ width <| fillPortion 7, height fill, alignTop ] tree
        , column
            [ width (fillPortion 3 |> maximum 500)
            , height fill
            , alignTop
            , padding 5
            , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
            , Border.color grey
            ]
          <|
            details
        ]


detailPanelContent : Plan -> List (Element Msg)
detailPanelContent plan =
    let
        attr : String -> String -> Element Msg
        attr name value =
            wrappedRow [ width fill ]
                [ el
                    [ width (px 200)
                    , paddingEach { right = 10, left = 10, top = 3, bottom = 3 }
                    , alignTop
                    ]
                  <|
                    text name
                , paragraph [ width fill, Font.bold, scrollbarX ] [ text value ]
                ]

        header : String -> Element Msg
        header name =
            el [ paddingEach { top = 10, bottom = 5, left = 10, right = 0 } ] <|
                el
                    [ Font.bold
                    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , Border.color lightGrey
                    ]
                <|
                    text name

        commonAttrs : CommonFields -> List (Element Msg)
        commonAttrs common =
            [ attr "Startup cost" <| String.fromFloat common.startupCost
            , attr "Total Cost" <| String.fromFloat common.totalCost
            , attr "Schema" common.schema
            ]
    in
    case plan of
        PCte cteNode ->
            commonAttrs cteNode.common

        PGeneric commonFields ->
            commonAttrs commonFields

        PResult resultNode ->
            commonAttrs resultNode.common

        PSeqScan seqScanNode ->
            commonAttrs seqScanNode.common
                ++ [ header "Filter"
                   , attr "Filter" seqScanNode.filter
                   , attr "Width" <| String.fromInt seqScanNode.rowsRemovedByFilter
                   ]

        PSort sortNode ->
            commonAttrs sortNode.common
                ++ [ header "Sort"
                   , attr "Sort Key" <| String.join ", " sortNode.sortKey
                   , attr "Sort Method" sortNode.sortMethod
                   , attr "Sort Space Type" sortNode.sortSpaceType
                   , attr "Sort Space Used" <| String.fromInt sortNode.sortSpaceUsed
                   ]



{-
   planNodeTree and childNodeTree are mutually recursive, so the end result of calling planNodeTree on the root node is that
   the whole plan tree gets rendered as elm-ui elements
-}


planNodeTree : Plan -> List (Element Msg)
planNodeTree plan =
    let
        nodeTypeEl : NodeType -> Element Msg
        nodeTypeEl nodeType =
            el [ Font.bold ] <| text nodeType

        treeNode : { treeNode | common : CommonFields } -> List (Element Msg) -> List (Element Msg)
        treeNode node nodeDetails =
            [ el
                [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color lightBlue
                , mouseOver [ Background.color lightYellow ]
                , padding 4
                , onMouseEnter <| MouseEnteredPlanNode plan
                , onMouseLeave <| MouseLeftPlanNode plan
                ]
              <|
                paragraph [] (nodeTypeEl node.common.nodeType :: nodeDetails)
            , childNodeTree node.common.plans
            ]
    in
    case plan of
        PCte cteNode ->
            treeNode cteNode
                [ text " on "
                , el [ Font.italic ] <| text cteNode.cteName
                , text <| " (" ++ cteNode.alias_ ++ ")"
                ]

        PGeneric genericNode ->
            treeNode { common = genericNode }
                []

        PResult resultNode ->
            treeNode resultNode
                []

        PSeqScan seqScanNode ->
            treeNode seqScanNode
                [ text " on "
                , el [ Font.italic ] <| text seqScanNode.relationName
                , text <| " (" ++ seqScanNode.alias_ ++ ")"
                ]

        PSort sortNode ->
            treeNode sortNode
                [ text " on "
                , el [ Font.italic ] <| text <| String.join ", " sortNode.sortKey
                ]


childNodeTree : Plans -> Element Msg
childNodeTree (Plans plans) =
    column [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ] <|
        List.concatMap planNodeTree plans


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
            { onChange = ChangeUserName
            , text = model.userName
            , label = Input.labelAbove [] <| text "User Name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = ChangePassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just StartLogin
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
