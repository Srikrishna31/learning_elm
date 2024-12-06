module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)



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


view : Model -> Document Msg
view model =
    let
        content =
            text "This isn't even my final form!"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ viewHeader model.page
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
            ul [] []
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
