module Picshare exposing (main)

import Browser
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

    In Elm applications, the model is responsible for containing all application state. This is different from other
    architectures such as MVC (Model-View-Controller) and MVVM (Model-View-ViewModel), or stuffing data in the DOM via
    data-* attributes. Those approaches encourage spreading your state across multiple models, making it hard to keep
    track of where state is located and how and when state changes. The Elm architecture allows you to know where your
    state is located because it's consolidated in one place.

    The definition for the Program type looks like this in Elm's internals:

    type Program flags model msg = Program

    Elm defines the Program type as a custom type with three type variables: flags, model, and msg.

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


{-|


# Handle State Changes

In MVC and MVVM applications, you can mutate models from almost anywhere in the codebase. This leads to the problem
of not knowing where or when state changes. The Elm Architecture solves this with its update process. Just as all
state is located in the model, all changes to the model have to take place in an update function.

The update function takes two arguments, a message and the model. The message argument comes from Elm's runtime in
response to events such as mouse clicks, server responses, and WebSocket events. The message describes the type of
state change. The update function is responsible for interpreting the message to change the state, so it must return
a new instance of the model with the changed state.


# The Elm Architecture Life Cycle

The Elm runtime takes the main program and bootstraps an initial application. It calls our view function with the initialModel
to produce a virtual DOM representation of the HTML to be displayed. Elm interprets the virtual DOM and renders the
correct HTML in the browser on our behalf.

Elm reads through the returned virtual DOM and encounters the event attribute, using the DOM API to wire up a click handler
on the button's DOM node. When you click on the button, the click handler will dispatch a message(Like in our case) to a
queue in the Elm runtime.

The Elm runtime will pick up the message from the queue and call update function with the message and current model, which
happens to be the initialModel at this moment. The update function will use the case expression to return a new model
with the `liked` field set to True.

The Elm runtime then calls the view function on the new model to retrieve a new virtual DOM representation. Elm compares
the current virtual DOM with the new virtual DOM and computes what's called a diff. A diff is basically a list of differences
between the old virtual DOM and the new virtual DON. During the diff process, Elm creates a list of patches to apply to
the real DOM in order to make it reflect the new virtual DOM. Diffs and patches are awesome because they give your
application better performance. The Elm runtime can avoid re-rendering the entire application and can instead add, remove
and replace DOM nodes only where necessary.

The above cycle repeats when the button is clicked again. Data flows in Elm applications are unidirectional. The data flows
in one direction from model to view to messages to update and back to model.

-}
update :
    Msg
    -> { url : String, caption : String, liked : Bool }
    -> { url : String, caption : String, liked : Bool }
update msg model =
    { model | liked = msg == Like }


main : Program () { url : String, caption : String, liked : Bool } Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
