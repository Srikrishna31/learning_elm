module PlanParsers.Json exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)


type Plan
    = PCte CteNode
    | PGeneric CommonFields
    | PResult ResultNode
    | PSeqScan SeqScanNode
    | PSort SortNode


type Plans
    = Plans (List Plan)


type alias NodeType =
    String


type alias CommonFields =
    { nodeType : NodeType
    , plans : Plans
    , relationName : String
    , schema : String
    , startupCost : Float
    , totalCost : Float
    }


type alias CteNode =
    { common : CommonFields
    , alias_ : String
    , cteName : String
    }


type alias ResultNode =
    { common : CommonFields
    , parentRelationShip : String
    }


type alias PlanJson =
    { executionTime : Float
    , plan : Plan
    , planningTime : Float
    , triggers : List String
    }


type alias SeqScanNode =
    { common : CommonFields
    , alias_ : String
    , filter : String
    , relationName : String
    , rowsRemovedByFilter : Int
    }


type alias SortNode =
    { common : CommonFields
    , sortKey : List String
    , sortMethod : String
    , sortSpaceUsed : Int
    , sortSpaceType : String
    }



{-
   succeed is a Json.Decode function which will kickoff the process of building the decoder. We pass it the constructor
   of the type we want to construct from JSON, and then provide definitions for each field with the help of the pipe
   operator.
   The order of fields here has to match the order of fields in the type alias. For each optional field we need to supply
   a default value of the right type.
-}


decodePlanJson : Decode.Decoder PlanJson
decodePlanJson =
    Decode.succeed PlanJson
        |> optional "Execution Time" Decode.float 0
        |> required "Plan" decodePlan
        |> optional "Planning Time" Decode.float 0
        |> optional "Triggers" (Decode.list Decode.string) []


decodeCommonFields : Decode.Decoder CommonFields
decodeCommonFields =
    Decode.succeed CommonFields
        |> required "Node Type" Decode.string
        |> optional "Plans" decodePlans (Plans [])
        |> optional "Relation Name" Decode.string ""
        |> optional "Schema" Decode.string ""
        |> required "Startup Cost" Decode.float
        |> required "Total Cost" Decode.float


decodePlans : Decode.Decoder Plans
decodePlans =
    Decode.map Plans <| Decode.list decodePlan


decodePlan : Decode.Decoder Plan
decodePlan =
    Decode.field "Node Type" Decode.string
        |> Decode.andThen decodeNode


decodeNode : String -> Decode.Decoder Plan
decodeNode nodeType =
    case nodeType of
        "CTE Scan" ->
            decodeCteNode

        "Result" ->
            decodeResultNode

        "Seq Scan" ->
            decodeSeqScanNode

        "Sort" ->
            decodeSortNode

        _ ->
            decodeGenericNode


decodeResultNode : Decode.Decoder Plan
decodeResultNode =
    let
        innerDecoder =
            Decode.succeed ResultNode
                |> custom decodeCommonFields
                |> required "Parent Relationship" Decode.string
    in
    Decode.map PResult innerDecoder


decodeCteNode : Decode.Decoder Plan
decodeCteNode =
    let
        innerDecoder =
            Decode.succeed CteNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> required "CTE Name" Decode.string
    in
    Decode.map PCte innerDecoder


decodeSeqScanNode : Decode.Decoder Plan
decodeSeqScanNode =
    let
        innerDecoder =
            Decode.succeed SeqScanNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> optional "Filter" Decode.string ""
                |> required "Relation Name" Decode.string
                |> optional "Rows Removed by Filter" Decode.int 0
    in
    Decode.map PSeqScan innerDecoder


decodeSortNode : Decode.Decoder Plan
decodeSortNode =
    let
        innerDecoder =
            Decode.succeed SortNode
                |> custom decodeCommonFields
                |> required "Sort Key" (Decode.list Decode.string)
                |> required "Sort Method" Decode.string
                |> required "Sort Space Used" Decode.int
                |> required "Sort Space Type" Decode.string
    in
    Decode.map PSort innerDecoder


decodeGenericNode : Decode.Decoder Plan
decodeGenericNode =
    Decode.map PGeneric decodeCommonFields
