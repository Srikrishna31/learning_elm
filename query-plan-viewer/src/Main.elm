module Main exposing (main)

import Attr
import Browser
import Color exposing (blue, darkGreen, green, grey, lightBlue, lightCharcoal, lightGrey, lightYellow, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as Input
import Json.Decode
import PlanParsers.Json as P exposing (..)


type Page
    = InputPage
    | DisplayPage


type Msg
    = SubmitPlan
    | ChangePlanText String
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan
    | ToggleMenu


type alias Model =
    { currPage : Page
    , currPlanText : String
    , selectedNode : Maybe Plan
    , isMenuOpen : Bool
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
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
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
    in
    { title = "VisExp"
    , body =
        [ layout [] <|
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


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
