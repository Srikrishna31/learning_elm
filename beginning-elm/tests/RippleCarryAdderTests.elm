module RippleCarryAdderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import RippleCarryAdder exposing (..)
import Test exposing (..)


inverterTests : Test
inverterTests =
    describe "Inverter"
        [ test "output is 0 when the input 0s 1" <|
            \_ ->
                inverter 0
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \_ ->
                inverter 1
                    |> Expect.equal 0
        ]


andGateTests : Test
andGateTests =
    describe "AND gate"
        [ test "output is 0 when both inputs are 0" <|
            \_ ->
                andGate 0 0
                    |> Expect.equal 0
        , test "output is 0 when the first input is 0" <|
            \_ ->
                andGate 0 1
                    |> Expect.equal 0
        , test "output is 0 when the second input is 0" <|
            \_ ->
                andGate 1 0
                    |> Expect.equal 0
        , test "output is 1 when both inputs are 1" <|
            \_ ->
                andGate 1 1
                    |> Expect.equal 1
        ]


orGateTests : Test
orGateTests =
    describe "OR Gate"
        [ test "output is 0 when both inputs are 0" <|
            \_ ->
                orGate 0 0
                    |> Expect.equal 0
        , test "output is 1 when the first input is 1" <|
            \_ ->
                orGate 1 0
                    |> Expect.equal 1
        , test "output is 1 when the second input is 1" <|
            \_ ->
                orGate 0 1
                    |> Expect.equal 1
        , test "output is 1 when both inputs are 1" <|
            \_ ->
                orGate 1 1
                    |> Expect.equal 1
        ]


halfAdderTests : Test
halfAdderTests =
    describe "Half adder"
        [ test "sum and carry-out are 0 when both inputs are 0" <|
            \_ ->
                halfAdder 0 0
                    |> Expect.equal { carry = 0, sum = 0 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 0 and the 2nd input is 1" <|
            \_ ->
                halfAdder 0 1
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 1 and the 2nd input is 0" <|
            \_ ->
                halfAdder 1 0
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 0 and carry-out is 1 when both inputs are 1" <|
            \_ ->
                halfAdder 1 1
                    |> Expect.equal { carry = 1, sum = 0 }
        ]


fullAdderTests : Test
fullAdderTests =
    describe "Full Adder"
        [ test "sum and carry-out are 0 when both inputs and carry-in are 0" <|
            \_ ->
                fullAdder 0 0 0
                    |> Expect.equal { carry = 0, sum = 0 }
        , test "sum is 1 and carry-out is 0 when both ipnuts are 0, but carry-in is 1" <|
            \_ ->
                fullAdder 0 0 1
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 0, the 2nd input is 1, and carry-in is 0" <|
            \_ ->
                fullAdder 0 1 0
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 0 and carry-out is 1 when the 1st input is 0, the 2nd input is 1, and the carry-in is 1" <|
            \_ ->
                fullAdder 0 1 1
                    |> Expect.equal { carry = 1, sum = 0 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 1, the 2nd input is 0, and the carry-in is 0" <|
            \_ ->
                fullAdder 1 0 0
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 0 and carry-out is 1 when the 1st input is 1, the 2nd input is 0, and the carry-in is 1" <|
            \_ ->
                fullAdder 1 0 1
                    |> Expect.equal { carry = 1, sum = 0 }
        , test "sum is 0 and carry-out is 1 when the 1st input is 1, the 2nd input is 1, and the carry-in is 0" <|
            \_ ->
                fullAdder 1 1 0
                    |> Expect.equal { carry = 1, sum = 0 }
        , test "sum is 1 and carry-out is 1 when the 1st input is 1, the 2nd input is 1, and the carry-in is 1" <|
            \_ ->
                fullAdder 1 1 1
                    |> Expect.equal { carry = 1, sum = 1 }
        ]
