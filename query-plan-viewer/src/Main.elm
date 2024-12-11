module Main exposing (main)

import Browser
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


type alias Model =
    { currPage : Page
    , currPlanText : String
    , selectedNode : Maybe Plan
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , currPlanText = ""
      , selectedNode = Nothing
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


blue : Color
blue =
    rgb255 52 101 164


lightCharcoal : Color
lightCharcoal =
    rgb255 136 138 133


green : Color
green =
    rgb255 0 255 0


white : Color
white =
    rgb255 255 255 255


darkGreen : Color
darkGreen =
    rgb255 0 150 0


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
            [ Background.color green
            , Border.color darkGreen
            , Border.rounded 3
            , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
            , Font.bold
            , Font.color white
            , paddingXY 20 6
            , alignRight
            , width (px 200)
            , height (px 40)
            ]
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


lightGrey : Color
lightGrey =
    rgb255 150 150 150


grey : Color
grey =
    rgb255 100 100 100


lightBlue : Color
lightBlue =
    rgb255 0 100 225


lightYellow : Color
lightYellow =
    rgb255 255 200 0



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
        , el [ alignRight ] <| text "Menu"
        ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
