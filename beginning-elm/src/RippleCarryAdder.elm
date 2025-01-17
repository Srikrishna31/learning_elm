module RippleCarryAdder exposing (..)

import Array exposing (Array)
import Bitwise


andGate : Int -> Int -> Int
andGate a b =
    Bitwise.and a b


orGate : Int -> Int -> Int
orGate a b =
    Bitwise.or a b


inverter : Int -> Int
inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            1


type alias OneBitAddRes =
    { carry : Int
    , sum : Int
    }


halfAdder : Int -> Int -> OneBitAddRes
halfAdder a b =
    let
        d : Int
        d =
            orGate a b

        e : Int
        e =
            andGate a b
                |> inverter

        sumDigit : Int
        sumDigit =
            andGate d e

        carryOut : Int
        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder : Int -> Int -> Int -> OneBitAddRes
fullAdder a b carryIn =
    let
        firstResult : OneBitAddRes
        firstResult =
            halfAdder b carryIn

        secondResult : OneBitAddRes
        secondResult =
            halfAdder a firstResult.sum

        finalCarry : Int
        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
    { carry = finalCarry
    , sum = secondResult.sum
    }


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


type alias AddResult =
    { carry : Int
    , sum0 : Int
    , sum1 : Int
    , sum2 : Int
    , sum3 : Int
    }


rippleCarryAdder : Int -> Int -> Int -> Int
rippleCarryAdder a b carryIn =
    let
        -- Extract digits
        firstSignal : Binary
        firstSignal =
            extractDigits a

        secondSignal : Binary
        secondSignal =
            extractDigits b

        -- Compute sum and carry-out
        firstResult : OneBitAddRes
        firstResult =
            fullAdder firstSignal.d3 secondSignal.d3 carryIn

        secondResult : OneBitAddRes
        secondResult =
            fullAdder firstSignal.d2 secondSignal.d2 firstResult.carry

        thirdResult : OneBitAddRes
        thirdResult =
            fullAdder firstSignal.d1 secondSignal.d1 secondResult.carry

        finalResult : OneBitAddRes
        finalResult =
            fullAdder firstSignal.d0 secondSignal.d0 thirdResult.carry
    in
    [ finalResult, thirdResult, secondResult, firstResult ]
        |> List.map .sum
        |> (::) finalResult.carry
        |> numberFromDigits


numberFromDigits : List Int -> Int
numberFromDigits digitsList =
    List.foldl (\digit number -> digit + 10 * number) 0 digitsList


extractDigits : Int -> Binary
extractDigits number =
    digits number
        |> padZeros 4
        |> Array.fromList
        |> arrayToRecord


padZeros : Int -> List Int -> List Int
padZeros total list =
    let
        numberOfZeros : Int
        numberOfZeros =
            total - List.length list
    in
    List.repeat numberOfZeros 0 ++ list


digits : Int -> List Int
digits number =
    let
        getDigits : Int -> List Int -> List Int
        getDigits n res =
            if n == 0 then
                res

            else
                getDigits (n // 10) (remainderBy 10 n :: res)
    in
    getDigits number []


arrayToRecord : Array Int -> Binary
arrayToRecord array =
    let
        getElement : Int -> Int
        getElement index =
            Array.get index array
                |> Maybe.withDefault -1

        firstElement : Int
        firstElement =
            getElement 0

        secondElement : Int
        secondElement =
            getElement 1

        thirdElement : Int
        thirdElement =
            getElement 2

        fourthElement : Int
        fourthElement =
            getElement 3
    in
    { d0 = firstElement
    , d1 = secondElement
    , d2 = thirdElement
    , d3 = fourthElement
    }
