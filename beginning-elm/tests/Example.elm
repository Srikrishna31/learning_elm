module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)



{- _ means any value, whereas () only unit type value.  If you just want to ignore a parameter without limiting the types
   of input values that can be passed to your function, then use _. On the other hand, if no meaningful argument will ever
   get passed to your function, then use ().
-}


suite : Test
suite =
    test "two plus two equals four"
        (\_ -> (2 + 2) |> Expect.equal 4)



{-
   # Maintaining a Suite of Well Written Tests

   A suite of well written tests play a critical role in refactoring the existing code. It gives us the confidence to
   rearrange our code so that we can make it easier to read and maintain. If we make a mistake, a test will catch it.
   For that reason, it's important to keep our test suite clean by removing any incorrectly written failing tests.
-}


guardianNames : Test
guardianNames =
    test "only 2 guardians have names with less than 6 characters" <|
        \_ ->
            let
                guardians =
                    [ "Star-lord", "Groot", "Gamora", "Drax", "Rocket" ]
            in
            guardians
                |> List.map String.length
                |> List.filter ((>) 6)
                |> List.length
                |> Expect.equal 2



-- # Grouping Similar Tests with `describe`


additionTests : Test
additionTests =
    describe "Addition"
        [ test "two plus two equals four duplicate" <|
            \_ -> (2 + 2) |> Expect.equal 4
        , test "three plus four equals seven" <|
            \_ -> (3 + 4) |> Expect.equal 7
        ]



-- # More Expectations


comparisonTests : Test
comparisonTests =
    describe "Comparison"
        [ test "2 is not equal to 3" <|
            \_ -> 2 |> Expect.notEqual 3
        , test "4 is less than 5" <|
            \_ -> 4 |> Expect.lessThan 5
        , test "6 is less than or equal to 7" <|
            \_ -> 6 |> Expect.atMost 7
        , test "9 is greater than 8" <|
            \_ -> 9 |> Expect.greaterThan 8
        , test "11 is greater than or equal to 10" <|
            \_ -> 11 |> Expect.atLeast 10
        , test "a list with zero elements is empty" <|
            \_ -> List.isEmpty [] |> Expect.equal True
        , test "a list with some elements is not empty" <|
            \_ ->
                List.isEmpty [ "Jyn", "Cassian", "K-2S)" ]
                    |> Expect.equal False
        ]
