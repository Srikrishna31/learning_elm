module MyFirstTest exposing (..)

import Expect as E
import Fuzz exposing (..)
import Main
import Test exposing (..)


suite =
    describe "My first tests"
        [ test "shrink text should work" testShrinkText
        , test "shrink text 2 should work" testShrinkText2
        , test "1 + 1 should be 2" test1Plus1
        , fuzz int "n + 0 should be n" testNPlusZero
        ]


testNPlusZero n =
    let
        result =
            n + 0

        ok =
            result == n
    in
    E.equal ok True


testShrinkText _ =
    let
        result =
            Main.shrinkText 20 "This is a test"

        ok =
            result == "This is a test"
    in
    E.equal ok True


testShrinkText2 _ =
    let
        result =
            Main.shrinkText 5 "This is a test"

        ok =
            result == "This  ..."
    in
    E.equal ok True


test1Plus1 _ =
    let
        result =
            1 + 1

        ok =
            result == 2
    in
    E.equal ok True
