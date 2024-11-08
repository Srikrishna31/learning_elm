module MyFirstTest exposing (..)

import Expect as E
import Test exposing (..)


test1 =
    test "1 + 1 should be 2" test1Plus1


test1Plus1 _ =
    let
        result =
            1 + 1

        ok =
            result == 2
    in
    E.true "1 + 1" ok
