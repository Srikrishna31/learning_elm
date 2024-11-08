module MyFirstTest exposing (..)

import Expect as E
import Fuzz exposing (..)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "My first tests"
        [ test "shrink text should work" testShrinkText
        , test "shrink text 2 should work" testShrinkText2
        , test "1 + 1 should be 2" test1Plus1
        , fuzz int "n + 0 should be n" testNPlusZero
        , fuzz int "n + n should be greater than n" testNGreater
        , fuzz string "shrink random text" testRandomShrink
        , fuzz (pair int string) "full shrink random text" testFullRandomShrinkText
        ]


testFullRandomShrinkText : ( Int, String ) -> E.Expectation
testFullRandomShrinkText ( max, text ) =
    let
        result =
            Main.shrinkText max text

        ok =
            String.length result <= max + 4
    in
    if max < 0 then
        E.pass

    else
        E.equal ok True


testRandomShrink : String -> E.Expectation
testRandomShrink text =
    let
        result =
            Main.shrinkText 5 text

        ok =
            String.length result <= 9
    in
    E.equal ok True


testNGreater : Int -> E.Expectation
testNGreater n =
    let
        result =
            n + n

        ok =
            result >= n
    in
    if n < 0 then
        E.pass

    else
        E.equal ok True


testNPlusZero : Int -> E.Expectation
testNPlusZero n =
    let
        result =
            n + 0

        ok =
            result == n
    in
    E.equal ok True


testShrinkText : () -> E.Expectation
testShrinkText _ =
    let
        result =
            Main.shrinkText 20 "This is a test"

        ok =
            result == "This is a test"
    in
    E.equal ok True


testShrinkText2 : () -> E.Expectation
testShrinkText2 _ =
    let
        result =
            Main.shrinkText 5 "This is a test"

        ok =
            result == "This  ..."
    in
    E.equal ok True


test1Plus1 : () -> E.Expectation
test1Plus1 _ =
    let
        result =
            1 + 1

        ok =
            result == 2
    in
    E.equal ok True
