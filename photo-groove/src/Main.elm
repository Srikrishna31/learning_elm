module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)



{-
   Single Page Application Architecture

   The "single page" in "single-page application" refers to a single page load- the browser loads the page only once. From
   the user's perspective, all the usual features of multiple pages still appear to work as normal: the URL in the address
   bar changes when clicking links, the browser's Back button still returns to the previous URL, and so on.
-}
{-
   The design of Nav.Key
   The reason Nav.Key exists is to restrict Nav.pushUrl to being used only by Elm programs started up by using Browser.application.
   Because the only way to obtain a Nav.Key is by having init provide one, and because only Browser.application has an
   init function that provides one, it is impossible to call Nav.pushUrl unless you originally started up by using
   Browser.application.
   This restriction exists because Elm's navigation APIs are designed with the assumption that they have total control
   over the page. That's what makes it safe for them to do things like overriding the default behavior for all the links
   on the page. If that assumption is violated-say, because we were using Browser.element to embed a small Elm application
   inside an existing single-page application written in JavaScript-then using pushUrl could result in some nasty bugs.
   The sole purpose of Nav.Key restriction on pushUrl is to make these bugs impossible. As such, the inner value of Nav.Key
   doesn't matter one bit. All that matters is that it's a token proving that the Elm Runtime has been started up by using
   Browser.application, and thus you can be confident it has full control over the page.
-}


type alias Model =
    { page : Page, key : Nav.Key }


type Page
    = SelectedPhoto String
    | Gallery
    | Folders
    | NotFound



{-
   Returning Document instead of HTML

   This Document Msg value being returned is a record with two fields:
   * title is a string that sets the page's title in the browser. Because we control the whole page now, we can do that.
   * body is a List (Html Msg) that specifies the children of the page's <body> element. It's a List rather than a single
   Html Msg node because we're controlling <body>'s entire list of children--whereas with Browser.element, we controlled a
   single element on the page.
-}
{-
   Calling Lazy on viewHeader
   Nearly everytime viewHeader is called, it will return the exact same Html Msg value as the previous time it was called.
   The only time it might possibly change is when model.page changes-because we navigated to a different page-in which
   case, it might return an Html Msg value with a different active link.
   lazy has this type:
   lazy : (arg -> Html msg) -> arg -> Html msg

   In other words, lazy expects a function that does the following:
    * Takes a single argument arg
    * Returns some Html

   If you want to use lazy with a function that takes multiple arguments, check out lazy2 and lazy3.

   Trying the first run
   The first time lazy viewHeader model.page gets run, laze will not improve our performance. In fact, it'll be almost as
   if we hadn't added lazy at all; lazy will call viewHeader, passing model.page, and return whatever viewHeader returned.
   However, as it does this, lazy also stores the value of model.page as well as the value viewHeader returned.
   The next time our program executes this line of code, lazy will first take a quick look at the model.page value we pass
   it. If it's the same model.page value that we passed last time, lazy won't bother calling viewHeader at all; instead,
   it will immediately return the result it stored previously.

   Guarantees that make skipping safe
   It's safe for lazy to skip calling viewHeader like this, thanks to two important guarantees:
   * When a call to Elm function gets the same arguments, it's guaranteed to return the same value.
   * Elm functions are guaranteed not to have side effects.

   Putting these together, if we already know what a function returned the last time we called it, then there's never any
   need to call that function again with those same arguments. Running the function again is guaranteed to do nothing
   more than return the value we already know.

   Using lazy isn't free
   lazy relies on storing extra information about every function that gets passed to it. Each time we call lazy, our
   program takes up more memory and does a bit more work to store the value and compare the result. If we used lazy
   everywhere, we'd run the risk of ballooning our program's memory usage and doing a bunch of unnecessary comparisons.
   We might make our overall performance considerably worse by doing this.

   This is why lazy is opt-in, and why it's best to use it only as needed.


   Understanding Debug.log
   The Debug.log function is unusual among Elm functions in that it actually performs a side effect every time we call it.
   Specifically, whenever we call Debug.log, the Elm Runtime immediately sends a String to the browser's console-completely
   bypassing the typical Elm Architecture way of handling effects through returning Cmd values from update. It's harmless
   to do this, because all Debug.log does is write to the browser console for development purposes, and no other part of
   an Elm program can depend on that console.

   Debug.log: String -> a -> a

   We pass it a String to display in the console, as well as a value (of any type) to be displayed right after that String,
   and it returns the exact same value we gave it. The typical way we'd use this is by starting with something like this:

   case msg of

   ...and then introducing a Debug.log like so:

   case Debug.log "Processing this msg" msg of

   Because Debug.log returns its second argument- in this case, msg -adding Debug.log like this has no effect on the code's
   behavior. This case will still operate on msg just as before.
-}


view : Model -> Document Msg
view model =
    let
        content =
            text "This isn't even my final form!"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { caption = "Gallery", url = "/gallery" } -- just to demonstrate the named arguments pattern.
                ]

        {-
           Preventing Argument Mix-ups
           navLink is defined to take the second argument as a record so that mixup of types can be avoided. When we pass
           an argument by record, the compiler should be able to help us with errors if arguments are passed in wrong order.

           Passing "Named Arguments"
           Anytime you have multiple arguments of the same type, and you're concerned about accidentally passing them in the
           wrong order, putting them into a record is a quick way to achieve "named arguments."  When using this pattern, it's
           common practice to destructure the arguments in the definition of the function:

           navLink targetPage {url, caption} =
           This way, they work and feel like normal arguments, but they are passed in with names instead of by position.
           If you want to destructure these fields, but also want to be able to access the original record in its entirety,
           you can use the as keyword to name the original record. For example, this would give the name config to the
           record while destructuring its fields: navLink targetPage ({url, caption} as config) =.
        -}
        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink targetPage { url, caption } =
            li [ classList [ ( "active", isActive { link = targetPage, page = page } ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


isActive : { link : Page, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ---------------------------------
        ( Gallery, Gallery ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, Folders ) ->
            True

        ( Folders, SelectedPhoto _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False

        ( NotFound, _ ) ->
            False


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]



{-
   Handling URL changes
    A common design goal of single-page applications is to maintain the URL bar's normal functionality. Ideally, it seems
    to the person using it as if they're using a multipage website, where links and the Back button work normally. The only
    noticeable difference is that things seem pleasantly snappie, because the SPA isn't doing a full page load for each
    new page.

    The onUrlRequest and onUrlChange functions in Browser.application help us to handle the url changes.

    Overriding Default Link Behavior
    First we have onUrlRequest = ClickedLink. This says that whenever the user requests a new URL (by clicking a link),
    Elm's runtime will send a ClickedLink message to our update function. It's like a page-wide event handler, except
    that it goes a step further by overriding the default behavior of all links on the page.
    With Browser.application, clicking links only sends this ClickedLink message it doesn't automatically load a new page
    as the browser normally would. This gives us total control over what we want the application to do when the user
    clicks a link.
    Note: Browser.application doesnot override the "open link in new tab" functionality. It also doesn't override the
    behavior of links with the download attribute set, because they're supposed to download things instead of navigating.

    Internal and External URL requests
    The Browser.UrlRequest value inside ClickedLink has two variants. It looks like this:

    type UrlRequest
        = External String
        | Internal Url

    Our ClickedLink message will contain an External request if the user clicked a link to a different domain. For example,
    if they're viewing any URL whose domain is elm-in-action.com and the user clicks a link to anywhere on manning.com,
    that would be an External request containing the exact string of the href attribute of the link they clicked.
    ClickedLink will contain an Internal request if the user clicked a link to the same domain. So if they're viewing
    an elm-in-action.com URL and they click a link to/foo, or to elm-in-action.com/foo, that will be an Internal request
    containing a Url record like the one init receives.

    Nav.load
    Nav.load performs a full page load, just as a traditional multipage app would do.

    Nav.pushUrl
    Whereas Nav.load does a full page load of an entirely new page, all pushUrl does is to push the given URL onto the
    browser's history stack. This has a few implications:
    * The URL shown in the browser's address bar will become this one.
    * Browser.application will send a ChangedUrl event to update, with this URL stored inside it. That's because we specified
    ChangedUrl for our onUrlChanged handler when we setup our Browser.application.
    * When the user clicks the Back button in the browser, this URL will now be one of the ones it goes back to. Also
    when the user does that, Browser.application will send a ChangedUrl event to update.

    Because it only manipulates the address bar, and does not directly request any page loads, we are free to display what
    appears to be a new page-without actually doing a real page load.
    Because both pushUrl and the browser's back button result in UrlChanged events being sent to update, we don't need
    separate handling code for the case where the user arrived at a given URL from pushUrl compared to using the Browser's
    back button. Once we implement handling code for the ChangedUrl message, we'll have covered both cases.

-}


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{-
   Returning Document instead of HTML
   Our main function is now calling Browser.document instead of the trusty Browser.element. The difference between the two
   is that in Browser.element, our view function must return Html Msg, whereas in Browser.document, our view function instead
   returns Document Msg. This gives our Elm application control over the entire page, whereas with Browser.element we were
   confined to a single DOM element on the page.

   Using Debug.TODO
   The Debug.todo function tells Elm's compiler, "I'm not ready to work on this part of my codebase yet, so for now pretend
   that there is some valid code". NO matter where you put it, a call to Debug.todo will type-check, because of its type:

   Debug.todo : String -> a
-}


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



{-
   A URL is a record representing different attributes about the URL such as its protocol (for example, "http" or "https"),
   host (eg. "elm-lang.org" or "elm-in-action.com"), and path(eg "/gallery" or "/photos/2turtles"). When our application
   starts up, init will be passed a Url value representing the current URL in the user's address bar when the page loaded.


    Understanding URL structure
    Single-page applications can get a lot of information out of a URL.

             Subdomain               Port            Query
            { \/ }                  {\/ }        {    \/    }
    https://stuff.elm-in-action.com:8042/zoo/car?stuff=things#panda
    { ^ }         {     ^         }     {   ^  }              { ^ }
    Protocol          Domain              Path                Path

    The elm/url package gives us three modules for working with the various pieces of a URL:

    * Url.Builder helps us assemble URLs from these separate ingredients.
    * Url.Parser and Url.Parser.Query help us translate path and query portions of a URL into more helpful values.
-}


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url, key = key }, Cmd.none )



{-
   Defining a Parser
    Similar to the way a JSON Decoder value describes how to translate a JSON string into a different type, this Parser
    value describes how to translate a URL into a Page.
    If we were decoding JSON into a Page, we would build a Decoder Page. However, our URL parser is not a Parser Page, but
    rather a Parser (Page -> a) a.
    To successfully build a single-page application, all we need to know is that Parser (Route -> a) -> a is the type of
    a Parser that translates a URL into a Route.

    Let's break down the implementation of below parser:

    Parser.map SelectedPhoto (s "photos" </> Parser.string)

    * This parser will succeed only if it is run on a URL whose path begins with the string "/photos" followed by a slash
    and then another string with a length of atleast 1. (So the URL path "/photos/" wouldnot match, but "/photos/a" would).
    * If it succeeds, the Parser's final output will be the String following the "photos/" part of the URL path. (So it
    would succeed with "puppy" when parsing a path of "/photos/puppy".)

    The </> operator expects a Parser value on each side, and the Parser.s function turns a hardcoded string into a parser
    for that string. For example, the parser (s "photos" </> Parser.string </> s "other" </> s "things") would match URLs
    like "/photos/foo/other/things".

    The Parser.map SelectedPhoto part works similar to the List.map, Result.map, Maybe.map and Decode.map functions: it
    transforms the value produced by the Parser. Without this call to Parser.map, our parser would output a plain old
    String whenever it succeeds. Thanks to Parser.map, that String will instead be passed along to SelectedPhoto, so the
    resulting parser will output a Page we can store in our model.

   Composing Parsers with Parser.OneOf
    * Parser.top matches with the root path. Parser.map Folders Parser.top returns a Parser that succeeds on a path of
    "/" and outputs a Folders value.
    * Parser.map Gallery (s "gallery"), which matches "/gallery" and outputs a Gallery value. Note that although we called
    the Parser.s function, this time we did not need to use the </> operator.
    * Parser.oneOf, which takes a List of parsers and tries them one at a time until it either finds a match or runs out.

   There's also a Json.Decode.oneOf that works similarly. It's useful for decoding JSON fields that situationally hold
   different types.
-}


parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]



{-
   Here's how the parser function works: Our Parser uses Parse.oneOf to try each of three Parsers-first, one that matches
   "/" and outputs Folders; then, one that matches "/gallery" and maps to Gallery; and finally, one that matches
   "/photos/something" and maps to SelectedPhoto with the string after "/photos" wrapped up inside it.
   If this parser succeeds then great! We have our Page and store it in the model. If it fails, then path was unrecognized,
   and we return the NotFound page by default.
-}


urlToPage : Url -> Page
urlToPage url =
    Debug.log "Parsing the provided URL" Parser.parse parser url
        |> Maybe.withDefault NotFound



{-
   Routing
   When the user visits a particular URL in the browser, our application will be notified about the URL they visited. Then
   it will inspect the URL and use its contents to decide which page to run. This process of determining which logic to run,
   based on the URL the user has visited, is called routing.
-}
