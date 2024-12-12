module PlanTree exposing (Config, render)

{-
   Structuring the code: Extracting Modules and Organizing Data
   When the code in a file grows too large, then there are four main approaches, in the order of complexity:
       * Extract helper functions into a module.
       * Extract a subset of view code into a module.
       * Extract non-UI functionality including state related functions into a module
       * Extract a subset of the model, update and view code into a module.

   The first approach is trivial, it is applied when PlanParsers.Json and Attr modules. The second approach allows us to
   extract reusable widget code into a separate module while leaving all of the logic to do with managing widget state
   and messages in place. The third approach addresses the issue of a growing Msg type and a complex update function along
   with extracting other functions. The last approach produces an approximation of what would be stateful components in
   other languages/frameworks, and is useful for managing larger code bases.
-}

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import PlanParsers.Json exposing (..)


type alias SelectedNode =
    Maybe Plan


type alias Config msg =
    { onMouseEnteredNode : Plan -> msg
    , onMouseLeftNode : Plan -> msg
    }


render : Config msg -> PlanJson -> SelectedNode -> Element msg
render config planJson selectedNode =
    let
        details =
            case selectedNode of
                Nothing ->
                    [ text "" ]

                Just plan ->
                    detailPanelContent plan
    in
    row [ width fill, paddingEach { top = 20, left = 0, right = 0, bottom = 0 } ]
        [ column [ width <| fillPortion 7, height fill, alignTop ] <|
            planNodeTree config planJson.plan
        , column
            [ width (fillPortion 3 |> maximum 500)
            , height fill
            , alignTop
            , padding 5
            , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
            , Border.color Color.grey
            ]
          <|
            details
        ]



{-
   planNodeTree and childNodeTree are mutually recursive, so the end result of calling planNodeTree on the root node is that
   the whole plan tree gets rendered as elm-ui elements
-}


planNodeTree : Config msg -> Plan -> List (Element msg)
planNodeTree config plan =
    let
        nodeTypeEl : NodeType -> Element msg
        nodeTypeEl nodeType =
            el [ Font.bold ] <| text nodeType

        treeNode : { treeNode | common : CommonFields } -> List (Element msg) -> List (Element msg)
        treeNode node nodeDetails =
            [ el
                [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color lightBlue
                , mouseOver [ Background.color lightYellow ]
                , padding 4
                , onMouseEnter <| config.onMouseEnteredNode plan
                , onMouseLeave <| config.onMouseLeftNode plan
                ]
              <|
                paragraph [] (nodeTypeEl node.common.nodeType :: nodeDetails)
            , childNodeTree config node.common.plans
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


childNodeTree : Config msg -> Plans -> Element msg
childNodeTree config (Plans plans) =
    column [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ] <|
        List.concatMap (planNodeTree config) plans


detailPanelContent : Plan -> List (Element msg)
detailPanelContent plan =
    let
        attr : String -> String -> Element msg
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

        header : String -> Element msg
        header name =
            el [ paddingEach { top = 10, bottom = 5, left = 10, right = 0 } ] <|
                el
                    [ Font.bold
                    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , Border.color lightGrey
                    ]
                <|
                    text name

        commonAttrs : CommonFields -> List (Element msg)
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
