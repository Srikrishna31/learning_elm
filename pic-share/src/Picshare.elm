module Picshare exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, i, img, input, li, strong, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)


type alias Id =
    Int


type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment


type alias Photo =
    { id : Id
    , url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }


type alias Model =
    Photo


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
viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton model
            , h2 [ class "caption" ] [ text model.caption ]
            , viewComments model
            ]
        ]


viewLoveButton : Model -> Html Msg
viewLoveButton model =
    let
        buttonClass =
            if model.liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div [ class "like-button" ]
        [ i
            [ class "fa fa-2x"
            , class buttonClass
            , onClick ToggleLike
            ]
            []
        ]


viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment: " ]
        , text <| " " ++ comment
        ]


viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        [] ->
            text ""

        _ ->
            div [ class "comments" ]
                [ ul [] <|
                    List.map viewComment comments
                ]


viewComments : Model -> Html Msg
viewComments model =
    div []
        [ viewCommentList model.comments
        , form [ class "new-comment", onSubmit SaveComment ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value model.newComment
                , onInput UpdateComment
                ]
                []
            , button [ disabled <| String.isEmpty model.newComment ]
                [ text "Save" ]
            ]
        ]


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


initialModel : Model
initialModel =
    { id = 1
    , url = baseUrl ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    , comments = [ "Cowabunga, dude!" ]
    , newComment = ""
    }


view : Model -> Html Msg
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
update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLike ->
            { model | liked = not model.liked }

        UpdateComment comment ->
            { model | newComment = comment }

        SaveComment ->
            saveNewComment model


saveNewComment : Model -> Model
saveNewComment model =
    let
        comment =
            String.trim model.newComment
    in
    case comment of
        "" ->
            model

        _ ->
            { model | comments = model.comments ++ [ comment ], newComment = "" }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


{-|

    Decode an object
    Let's understand decoding json objects with a simple dog json record. Enter the following in the REPL:
    > dog name age = {name = name, age = age}
    <function> : a -> b -> { age: b , name: a }

    Create a dog decoder by entering the following code in the REPL:
    > dogDecoder =
    |   succeed dog
    |       |> required "name" string
    |       |> required "age" int
    <internals>: Json.Decode.Decoder {age: Int, name: String}

    Let's understand the function line by line. The succeed function creates a decoder literal. For example you call
    succeed on the string "Elm", then you get back a Decoder String. For the dog function, you get back a
    Decoder (a -> b -> {age: b, name: a}). Essentially you get back a decoder of whatever you pass in, even if it's a
    function like dog.
    On the next line, we use the pipe operator to feed the decoder into the required function. It requires a property to
    exist in the JSON object just like the field function. It's different from field in that it not only extracts the
    property but also applies the value to the function inside the current decoder. Below is the type signature of required:

        required: String -> Decoder a -> Decoder b -> Decoder (a -> b) -> Decoder b

    The first argument is a String, which is the name of the property. The second argument is a Decoder a that expects
    the property to have a type of a. The third argument is another decoder that contains a function. This inner function
    must translate the type a to the type b. This translation process allows required to return a Decoder b.

    In this example, the third argument is the decoder that contains the dog function. If you had only run the first two
    lines from the example, the decoder would have this type:

        Decoder ( a -> {age: a, name: String})

    Compare that type to executing only the first line of the example:

        Decoder (a -> b -> {age: b, name: a})

    Notice that you filled in the first type variable to be a String. That is, you went from a function with two arguments
    to a function with one argument.

    Moving to the third line in the example, you call the required function with the "age", the int decoder, and the
    current dog decoder. The dog decoder can now extract the age property and apply it as the second argument to the
    original dog function, which gives us the following final decoder:

        Decoder {age: Int, name: String}

    The photoDecoder resembles the dogDecoder written in the REPL with a couple of differences. First is the call to
    succeed on the Photo constructor function. You pipe the constructor function through several calls to required with
    different decoders. For the "id" property you use the int decoder. For the "url" and "caption" properties, you use
    the string decoder. For the "liked" property you use the bool decoder. Finally, for the "comments" property you use
    list string. The list decoder takes another decoder as an argument to decode each item in the JSON array to that
    inner decoder's type.
    At the end, you use the hardcoded function. You can use the hardcoded function to tell the decoder to use a static
    value as an argument to the underlying decoder function instead of extracting a property from the JSON object. In
    this case, you use hardcoded to provide the empty string as the final newComment argument to the Photo constructor
    function.

    One important note to add is that the order of the piping operations matters. The order needs to match the order of
    the arguments to the constructor function. For example, if you switched the order of the id and url field decoders,
    you would get a compiler error. That's because the decoder would think it needs to call the constructor function with
    a String first instead of an Int.

-}
photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "id" int
        |> required "url" string
        |> required "caption" string
        |> required "liked" bool
        |> required "comments" (list string)
        |> hardcoded ""
