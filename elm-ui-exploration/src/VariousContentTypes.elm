module VariousContentTypes exposing (..)

import Arithmetic
import Colors exposing (blue, darkCharcoal, lightBlue, lightGrey)
import Element exposing (Attribute, Color, Element, alignTop, column, el, fill, fromRgb, height, html, htmlAttribute, image, layout, newTabLink, none, padding, paddingXY, paragraph, rgb, rgb255, rgba, row, scrollbarX, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy
import Element.Region as Region
import Html exposing (Html, code)
import Html.Attributes
import Mark exposing (Block, Document)
import Mark.Error
import Markdown.Block as Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Palette.Cubehelix as Palette
import SolidColor exposing (SolidColor)
import Utils exposing (layoutWithPadding)



{-
   Performance Optimization

   Element.Keyed allows you to provide a string identifier for child elements, which makes it possible for the diffing
   algorithm to reuse nodes between renders. This can improve performance in situations where child elements are added,
   removed or reordered (typically in a list of some kind).

   Element.Keyed provides three functions:

   el: List (Attribute msg) -> (String, Element msg) -> Element msg
   column: List (Attribute msg) -> List (String, Element msg) -> Element msg
   row: List (Attribute msg) -> List (String, Element msg) -> Element msg

   The difference with their regular counterparts from the Element model is that they take tuples of (String, Element msg).
   You have to ensure that string IDs are unique.

   Note that there are no keyed versions of other container elements like table or wrappedRow.
-}


colorList : Bool -> Html msg
colorList isReversed =
    layoutWithPadding <|
        let
            colorRow clr =
                -- Each row is associated with an ID which is the hex value of the colour
                ( SolidColor.toHex clr
                  --uniqueID made from color value
                , row [ width fill, spacing 20 ]
                    --associated element
                    [ el
                        [ width fill
                        , height fill
                        , Background.color <| solidColorAsColor clr
                        ]
                        none
                    , el [ width fill ] <| text <| SolidColor.toHex clr
                    ]
                )
        in
        Keyed.column [ width fill, spacing 30 ] <|
            -- The children are passed in as a list of id + element tuples
            ( "Header"
            , row [ width fill, spacing 20, Font.bold ]
                [ el [ width fill ] <| text "Color"
                , el [ width fill ] <| text "Value"
                ]
            )
                :: (List.map colorRow <| palette isReversed)


solidColorAsColor : SolidColor -> Color
solidColorAsColor clr =
    let
        ( r, g, b ) =
            SolidColor.toRGB clr
    in
    fromRgb <| { red = r / 255, green = g / 255, blue = b / 255, alpha = 1.0 }


palette : Bool -> List SolidColor
palette isReversed =
    Palette.generate 256
        |> List.sortBy
            (if isReversed then
                SolidColor.toHex >> String.reverse

             else
                SolidColor.toHex
            )



{-
      Lazy Evaluation
   Element.Lazy allows you to avoid re-rendering the view when the model is unchanged. This module caters to functions of
   upto five arguments (whereas Html.Lazy goes all the way to 8 arguments):

   lazy: (a -> Element msg) -> a -> Element msg

   lazy2 : (a -> b -> Element msg) -> a -> b -> Element msg

   lazy3 : (a -> b -> c -> Element msg) -> a -> b -> c -> Element msg

   lazy4: (a -> b -> c -> d -> Element msg) -> a -> b -> c -> d -> Element msg

   lazy5: (a -> b -> c -> d -> e -> Element msg) -> a -> b -> c -> d -> e -> Element msg


   The optimization is based on the fact that Elm functions are pure. Calling view model results in generating a virtual
   DOM object which could potentially get very large. lazy* functions bundle the view function and its arguments without
   actually calling it.

   The virtual DOM diffing algorithm compares the view function arguments by reference (which is really fast), and if they
   are the same, it skips calling the view function altogether.

   One needs to be careful with the arguments of lazy* functions. Anything that prevents argument comparison by reference
   will remove the speedup. For example, if instead of plain integer arguments you passed a newly-constructed list of limits
   or a record to primesBelow, it would mean that reference equality checks would fail as these values are not the same as
   whatever was passed below:

   primesBelow [1000 2000 3000] -- would not work with Element.Lazy.lazy
   primesBelow {limit = 1000000, interval = 200000 } -- would not work either

   So, if you need to pass a value which is not an Int, Float, Bool, String or Char, store it in the model and pass it that
   way rather than constructing the argument in place. That way, as long as the value in the model doesn't change, reference
   equality check will succeed.

   Similarly, if you passed a lambda instead of a named function, it would be a problem as well:

    Lazy.lazy2 (\limit interval -> primesBelow limit interval) 1000000 200000

    So always pass a named function to lazy*

-}


primesBelow : Int -> Int -> Element msg
primesBelow limit interval =
    let
        intervalCount =
            limit // interval
    in
    List.range 1 intervalCount
        |> List.map ((*) interval)
        |> List.map
            (\lmt ->
                text
                    ((String.fromInt <| List.length <| Arithmetic.primesBelow lmt)
                        ++ " primes less than "
                        ++ String.fromInt lmt
                    )
            )
        |> column [ spacing 20 ]


lazyRenderingExample : Html msg
lazyRenderingExample =
    layoutWithPadding <|
        column [ spacing 30 ]
            [ el [ Font.bold ] <| text "Number of primes:"
            , el
                [ Background.color lightBlue, padding 20 ]
              <|
                Element.Lazy.lazy2 primesBelow 1000000 200000

            --recomputed on every color change, if not for the lazy construct
            ]



{-
   We use a powerful package called dillonkearns/elm-markdown. This package allows you to define a custom renderer capable
   of producing any kind of "view" value rather than just Html msg.

   The Markdown string is passed to Markdown.Parser.parse which can either succeed or fail. If it fails, it produces a
   list of errors. If it succeeds, it produces a list of Markdown.Block.Block values, which can then go through a renderer,
   and be turned into Element msg values.
-}


markdownView : String -> Result String (List (Element msg))
markdownView markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError
            (\error ->
                error |> List.map Markdown.Parser.deadEndToString |> String.join "\n"
            )
        |> Result.andThen (Markdown.Renderer.render elmUiRenderer)


elmUiRenderer : Markdown.Renderer.Renderer (Element msg)
elmUiRenderer =
    { heading = heading
    , paragraph = paragraph [ spacing 15 ]
    , thematicBreak = none
    , text = text
    , strong = \content -> row [ Font.bold ] content
    , emphasis = \content -> row [ Font.italic ] content
    , strikethrough = \content -> row [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            newTabLink
                [ htmlAttribute <| Html.Attributes.style "display" "inline-flex" ]
                { url = destination
                , label =
                    paragraph [ Font.color blue ] body
                }
    , hardLineBreak = html <| Html.br [] []
    , image =
        \img ->
            image [ width fill ] { src = img.src, description = img.alt }
    , blockQuote =
        \children ->
            column
                [ padding 10
                , Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Border.color darkCharcoal
                , Background.color lightGrey
                ]
                children
    , unorderedList =
        \items ->
            column [ spacing 15 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            row [ spacing 5 ]
                                [ row
                                    [ alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Input.defaultCheckbox False

                                        CompletedTask ->
                                            Input.defaultCheckbox True

                                        NoTask ->
                                            text "•"
                                     )
                                        :: text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            column [ spacing 15 ] <|
                List.indexedMap
                    (\index itemBlocks ->
                        row [ spacing 5 ]
                            [ row [ alignTop ] <|
                                text
                                    (String.fromInt (index + startingIndex) ++ " ")
                                    :: itemBlocks
                            ]
                    )
                    items
    , codeBlock = codeBlock
    , html = Markdown.Html.oneOf []
    , table = column []
    , tableHeader = column []
    , tableBody = column []
    , tableRow = row []
    , tableHeaderCell = \maybeAlignment children -> paragraph [] children
    , tableCell = \maybeAlignment children -> paragraph [] children
    }


heading : { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    paragraph
        [ Font.size
            (case level of
                Block.H1 ->
                    36

                Block.H2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Region.heading <| Block.headingLevelToInt level
        ]
        children


code : String -> Element msg
code snippet =
    el
        [ Background.color lightGrey
        , Border.rounded 2
        , paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
    <|
        text snippet


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    el
        [ width fill
        , scrollbarX
        , Background.color (rgba 0 0 0 0.03)
        , htmlAttribute (Html.Attributes.style "white-space" "pre")
        , padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
    <|
        text details.body


markdownBody : String
markdownBody =
    """# Markdown Sample
    With the `dillonkearns/elm-markdown` package, it's possible to turn *Markdown* straight into
    `Element msg` values:

    markdownView : String -> Result String (List (Element msg))
    markdownView markdown =
        markdown
            |> Markdown.Parser.parse
            |> Result.mapError
                (\\error ->
                    error |> List.map Markdown.Parser.deadEndToString |> String.join "\\n"
                )
            |> Result.andThen (Markdown.Renderer.render elmUiRenderer)


    [dillonkearns/elm-markdown documentation](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/)

    An image for good measure:

    ![A random image](https://picsum.photos/800/400)
    """


markdownExample : Html msg
markdownExample =
    layoutWithPadding <|
        case markdownView markdownBody of
            Ok renderedEls ->
                column [ spacing 30, padding 10, width fill ] renderedEls

            Err string ->
                text string



{-
   Elm-markup

   elm-markup parser functions return Block values which can be composed in order to handle more complex documents. The
   text function produces a block containing a list of output values, rather than a single value. Since styledText converts
   a style record and a string into an el with some font attributes, the type of output values is Element msg.
-}


styledText : { bold : Bool, italic : Bool, strike : Bool } -> String -> Element msg
styledText styles str =
    let
        when : Bool -> Attribute msg -> List (Attribute msg) -> List (Attribute msg)
        when cond value list =
            if cond then
                value :: list

            else
                list
    in
    text str
        |> el
            ([]
                |> when styles.bold Font.bold
                |> when styles.italic Font.italic
                |> when styles.strike Font.strike
            )


inlineMarkup : Block (List (Element msg))
inlineMarkup =
    Mark.text styledText


document : Document (Element msg)
document =
    Mark.document (paragraph []) inlineMarkup


markup : String
markup =
    """
    Let's look at some *styled* /markup/.
    """


markupView : Html msg
markupView =
    let
        errorsToEl errors =
            errors
                |> List.map (Mark.Error.toString >> text)
                |> List.map (\txtEl -> paragraph [] [ txtEl ])
                |> textColumn []

        markupToEl =
            case Mark.compile document markup of
                Mark.Success el ->
                    el

                Mark.Almost { result, errors } ->
                    -- This is the case where there has been an error, but it has been caught by `Mark.onError` and is
                    -- still renderable.
                    column []
                        [ errorsToEl errors
                        , result
                        ]

                Mark.Failure errors ->
                    errorsToEl errors
    in
    layoutWithPadding markupToEl
