-- Elm modules must match their filenames.


module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (..)
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

   Fuzz Tests
   Elm's fuzz tests are tests that run several times with randomly generated inputs. Outside Elm, this testing style is
   sometimes called fuzzing, generative testing, property-based testing, or Quickcheck-style testing.
   A common way to write a fuzz test is to start by writing a unit test and then convert it to a fuzz test to help identify
   edge cases.
-}


decoderTest : Test
decoderTest =
    {-
       The call to fuzz2 says that we want a fuzz test that randomly generates two values. string and int are two fuzzers
       specifying that we want the first generated value to be a String, and the second to be an integer. Their types are
       string: Fuzzer String and int: Fuzzer Int.

       A fuzzer specifies how to randomly generate values for fuzz tests.
       Note: Fuzz.string doesnot generate strings completely at random. It has a higher probability of generating values
       that are likely to cause bugs: the empty string, very short strings, and very long strings. Similarly, Fuzz.int
       prioritizes generating 0, a mix of positive and negative numbers, and a mix of very small and very large numbers.
       Other fuzzers tend to be designed with similar priorities.
    -}
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                {-
                   .title is equivalent to this anonymous function: (\photo -> photo.title)
                   All the records with named fields get a function with the same name that return their
                   contents, which is a shorthand for the anonymous function accepting a record and returning the needed
                   field's value.
                -}
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitled)")



{-
   Building JSON programmatically with Json.Encode
   Json.Encode.Value
   Whereas the Json.Decode module centers around the Decoder abstraction, the Json.Encode module centers around the Value
   abstraction. A Value (short for Json.Encode.Value) represents a JSON-like structure. In our case, we will use it to
   represent actual JSON, but it can represent objects from JavaScript as well.

   Encode.int : Int ->                    Value
   Encode.string: String ->               Value
   Encode.object: List (String, Value) -> Value
-}
{-
   Testing update functions
    All Elm programs share some useful propeties that make them easier to test:
        * The entire application state is represented by a single Model value.
        * Model changes only when update receives a Msg and returns a new Model.
        * update is a plain old function, so we can call it from tests like any other function.
-}


slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount
