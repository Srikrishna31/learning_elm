module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, i, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)


type Msg
    = Like
    | Unlike


{-| A compiled Elm application creates a global Elm namespace variable. The Elm variable has properties for any top-level
modules compiled.
Every compiled module has an init function that accepts a configuration object. The node property of the configuration
object specifies a DOM node.
-}
main : Html Msg
main =
    view initialModel


{-|

    In Elm applications, the model is responsible for containing all application state. This is different from other
    architectures such as MVC (Model-View-Controller) and MVVM (Model-View-ViewModel), or stuffing data in the DOM via
    data-* attributes. Those approaches encourage spreading your state across multiple models, making it hard to keep
    track of where state is located and how and when state changes. The Elm architecture allows you to know where your
    state is located because it's consolidated in one place.

-}
viewDetailedPhoto : { url : String, caption : String, liked : Bool } -> Html Msg
viewDetailedPhoto { url, caption, liked } =
    let
        buttonClass : String
        buttonClass =
            if liked then
                "fa-heart"

            else
                "fa-heart-o"

        msg : Msg
        msg =
            if liked then
                Unlike

            else
                Like
    in
    div [ class "detailed-photo" ]
        [ img [ src url ] []
        , div [ class "photo-info" ]
            [ div [ class "like-button" ]
                [ i
                    [ class "fa fa-2x"
                    , class buttonClass
                    , onClick msg
                    ]
                    []
                ]
            , h2 [ class "caption" ] [ text caption ]
            ]
        ]


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


initialModel : { url : String, caption : String, liked : Bool }
initialModel =
    { url = baseUrl ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    }


view : { url : String, caption : String, liked : Bool } -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model ]
        ]



{-

   # Handle State Changes

   In MVC and MVVM applications, you can mutate models from almost anywhere in the codebase. This leads to the problem
   of not knowing where or when state changes. The Elm Architecture solves this with its update process. Just as all
   state is located in the model, all changes to the model have to take place in an update function.

   The update function takes two arguments, a message and the model. The message argument comes from Elm's runtime in
   response to events such as mouse clicks, server responses, and WebSocket events. The message describes the type of
   state change. The update function is responsible for interpreting the message to change the state, so it must return
   a new instance of the model with the changed state.

-}
