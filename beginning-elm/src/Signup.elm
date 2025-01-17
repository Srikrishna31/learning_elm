module Signup exposing (..)

import Browser
import Css exposing (..)
import Html exposing (Attribute, Html, button, div, h1, input, text)
import Html.Attributes exposing (id, style, type_)


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
        [ h1 [ style "padding-left" "3cm" ] [ text "Sign up" ]
        , Html.form formStyle
            [ div []
                [ text "Name"
                , input ([ id "name", type_ "text" ] ++ inputStyle) []
                ]
            , div []
                [ text "Email"
                , input ([ id "email", type_ "email" ] ++ inputStyle) []
                ]
            , div []
                [ text "Password"
                , input ([ id "password", type_ "password" ] ++ inputStyle) []
                ]
            , div []
                [ button ([ type_ "submit" ] ++ buttonStyle)
                    [ text "Create my account" ]
                ]
            ]
        ]


init : User
init =
    initialModel


main : Program () User Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


update : Msg -> User -> User
update msg user =
    user


formStyle : List (Attribute msg)
formStyle =
    [ style "border-radius" "5px"
    , style "background-color" "#f2f2f2"
    , style "padding" "20px"
    , style "width" "300px"
    ]


inputStyle : List (Attribute msg)
inputStyle =
    [ style "display" "block"
    , style "width" "260px"
    , style "padding" "12px 20px"
    , style "margin" "8px 0"
    , style "border" "none"
    , style "border-radius" "4px"
    ]


buttonStyle : List (Attribute msg)
buttonStyle =
    [ style "width" "300px"
    , style "background-color" "#397cd5"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-top" "10px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "16px"
    ]
