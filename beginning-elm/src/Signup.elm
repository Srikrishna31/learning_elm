module Signup exposing (..)

--import Html exposing (Attribute, Html, button, div, h1, input, text)
--import Html.Attributes exposing (id, style, type_)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


type Msg
    = Dummy


type alias User =
    { name : String
    , email : String
    , password : String
    , loggedIn : Bool
    }


initialModel : User
initialModel =
    { name = ""
    , email = ""
    , password = ""
    , loggedIn = False
    }



{-
   Styling Our View
       There are multiple ways to style a page in Elm:
           * Using inline styles
           * Using the elm-css package
           * Using an external CSS file
           * Using a CSS framework

      The best practice for working with HTML suggests that the styles should primarily be specified in CSS files.
      Therefore, we shouldn't use too many inline styles.
-}


view : User -> Html msg
view user =
    div []
        [ h1 [ css [ paddingLeft <| cm 3 ] ] [ text "Sign up" ]
        , styledForm []
            [ div []
                [ text "Name"
                , styledInput [ id "name", type_ "text" ] []
                ]
            , div []
                [ text "Email"
                , styledInput [ id "email", type_ "email" ] []
                ]
            , div []
                [ text "Password"
                , styledInput [ id "password", type_ "password" ] []
                ]
            , div []
                [ styledButton [ type_ "submit" ]
                    [ text "Create my account" ]
                ]
            ]
        ]



{-
   Html.Styled is a drop-in replacement for the Html module from the elm/html package. All it does is return a styled
   version of elements defined in the html module.
   For example, here is how the Html.text function is implemented:

   text: String -> Html msg
   text =
        VirtualDom.text

   Here is how the Html.Styled.text is implemented:

   text: String -> Html msg
   text =
        VirtualDom.Styled.text

   To style an element using elm-css, we need to use the styled function defined in the Html.Styled module. here is how
   it's type signature looks:

    styled:
        (List (Attribute a) -> List (Html b) -> Html msg)
         -> List Style
         -> List (Attribute a)
         -> List (Html b)
         -> Html msg
    The first argument is an HTML element from the Html.Styled module. The second argument is a list of CSS styles.
-}


styledForm : List (Attribute msg) -> List (Html msg) -> Html msg
styledForm =
    styled Html.Styled.form
        [ borderRadius <| px 5
        , backgroundColor <| hex "#f2f2f2"
        , padding <| px 20
        , Css.width <| px 300
        ]


styledInput : List (Attribute msg) -> List (Html msg) -> Html msg
styledInput =
    styled Html.Styled.input
        [ display block
        , Css.width <| px 260
        , padding2 (px 12) <| px 20
        , margin2 (px 8) <| px 0
        , border <| px 0
        , borderRadius <| px 4
        ]


styledButton : List (Attribute msg) -> List (Html msg) -> Html msg
styledButton =
    styled Html.Styled.button
        [ Css.width <| px 300
        , backgroundColor <| hex "#397cd5"
        , color <| hex "#fff"
        , padding2 (px 12) <| px 20
        , marginTop <| px 10
        , border <| px 0
        , borderRadius <| px 4
        , fontSize <| px 16
        ]


init : User
init =
    initialModel


main : Program () User Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = \model -> toUnstyled <| view model
        , update = update
        }


update : Msg -> User -> User
update msg user =
    user
