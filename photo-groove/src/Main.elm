module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)



{-
   Single Page Application Architecture

   The "single page" in "single-page application" refers to a single page load- the browser loads the page only once. From
   the user's perspective, all the usual features of multiple pages still appear to work as normal: the URL in the address
   bar changes when clicking links, the browser's Back button still returns to the previous URL, and so on.
-}


type alias Model =
    { page : Page }


type Page
    = Gallery
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
            li [ classList [ ( "active", page == targetPage ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{-
   Returning Document instead of HTML
   Our main function is now calling Browser.document instead of the trusty Browser.element. The difference between the two
   is that in Browser.element, our view function must return Html Msg, whereas in Browser.document, our view function instead
   returns Document Msg. This gives our Elm application control over the entire page, whereas with Browser.element we were
   confined to a single DOM element on the page.
-}


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { page = Gallery }, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
