module RippleCarryAdder exposing (..)

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
        d =
            orGate a b

        e =
            andGate a b
                |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder : Int -> Int -> Int -> OneBitAddRes
fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

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


rippleCarryAdder : Binary -> Binary -> Int -> AddResult
rippleCarryAdder a b carryIn =
    let
        firstResult : OneBitAddRes
        firstResult =
            fullAdder a.d3 b.d3 carryIn

        secondResult : OneBitAddRes
        secondResult =
            fullAdder a.d2 b.d2 firstResult.carry

        thirdResult : OneBitAddRes
        thirdResult =
            fullAdder a.d1 b.d1 secondResult.carry

        finalResult : OneBitAddRes
        finalResult =
            fullAdder a.d0 b.d0 thirdResult.carry
    in
    { carry = finalResult.carry
    , sum0 = finalResult.sum
    , sum1 = thirdResult.sum
    , sum2 = secondResult.sum
    , sum3 = firstResult.sum
    }
