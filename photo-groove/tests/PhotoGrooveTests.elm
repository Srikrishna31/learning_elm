-- Elm modules must match their filenames.


module PhotoGrooveTests exposing (..)

import Common exposing (urlPrefix)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGallery exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)



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
                |> decodeValue PhotoGallery.photoDecoder
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
    All Elm programs share some useful properties that make them easier to test:
        * The entire application state is represented by a single Model value.
        * Model changes only when update receives a Msg and returns a new Model.
        * update is a plain old function, so we can call it from tests like any other function.

    slidHueSetsHue : Test
    slidHueSetsHue =
        fuzz int "SlidHue sets the hue" <|
            \amount ->
                initialModel
                    |> update (SlidHue amount)
                    |> Tuple.first
                    |> .hue
                    |> Expect.equal amount
-}


sliders : Test
sliders =
    describe "Slider tests the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount



{-
   Testing views
   * Initially, we don't render any thumbnails.
   * Once the photos load, we render a thumbnail for each of them.
   * When you click a thumbnail, that photo becomes selected.

   Querying HTML
   Query functions make use of two types as they descend into Html:
   * Query.Single, which represents a single DOM node
   * Query.Multiple, which represents multiple DOM nodes.

   The Query.fromHtml function begins the process of descending into an Html value, by returning a Single representing
   the Html's root node. Below is the type of Query.fromHtml:

   fromHtml: Html msg -> Query.Single msg
-}


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsWork : Test
thumbnailsWork =
    fuzz urlFuzzer "URLS render as thumbnails" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel
                | status = Loaded (List.map photoFromUrl urls) ""
            }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")



{-
    Choosing which photo to click
    Generating random numbers is an effect, and elm-test tests are not permitted to run effects. What we can do instead
    is to generate two lists at random: one list to go before the URL we'll click and another to go after it.
   HTML.Test.Event
   This gives us access to a function called Event.simulate that simulates user events, such as clicks, and checks
   whether they result in the expected message being sent to update.
-}


clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)
