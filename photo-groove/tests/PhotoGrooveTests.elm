-- Elm modules must match their filenames.


module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import PhotoGroove
import Test exposing (..)



{-
   In Elm, a unit test is a test that runs once, and whose test logic doesnot perform effects.

   Expectations
   Every unit test requires a single expression that evaluates to an Expectation value.
   Expect.equal is a function that has the following type:

    equal: a -> a -> Expectation

   We pass it two values, and it returns an Expectation that claims the two values are equal. If they turn out to be equal
   the expectation will pass. If not, it will fail. Failed expectations usually translate to failed tests.

   The first argument to test is a description of the test. If the test fails, elm-test will display this text to let us
   know which test failed. Because the purpose of these descriptions is to help us identify which test failed, elm-test
   enforces that they must be unique.

   The second argument to test is not an actual Expectation value, but rather an anonymous function that returns an Expectation
   This function wrapper is important, but unit tests never need to reference the argument it receives, so we can always
   safely disregard that argument by naming it "_". Tests always expect a wrapper anonymous function, to postpone
   evaluation of the test.
-}


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)" <|
        \_ ->
            """{"url": "fruits.com", "size":5}"""
                |> decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)" })
