module VariousContentTypes exposing (..)

import Arithmetic
import Colors exposing (lightBlue)
import Element exposing (Color, Element, column, el, fill, fromRgb, height, none, padding, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Keyed as Keyed
import Element.Lazy
import Html exposing (Html)
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
