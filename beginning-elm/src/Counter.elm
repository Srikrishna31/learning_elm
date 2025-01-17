module Counter exposing (..)

{-
   On a very high level, web applications tend to have two major parts: state and user interface (UI). An application starts
   with an initial state and presents that state to the user through UI. The user takes some action through a UI element
   that modifies the initial state. The new state is then presented back to the user for more actions.
   At any given point in time, an application needs to store different types of information in memory. For example, it
   needs to know whether or not a user is logged in or how many blogs a user has posted. State is like a repository for
   storing all of this information. This state is then made available to various data structures in the application.
   Functions in the application perform different operations on it, resulting into a new state.

   # Model
   In Elm, we represent the state of an application with something called a model. A model is just a data structure that
   contains important information about the application.
-}

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    Int


initialModel : Model
initialModel =
    0



{-
   # View
   The view function takes a model and returns HTML code. Behind the scenes, the div function in Elm produces the <div>
   element in HTML, and the button function produces the <button> element. The text function doesn't represent any HTML
   tag. It just displays a plain text by escaping special characters so that the text appears exactly as we specify in
   our code.
-}


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (String.fromInt model)
        , button [ onClick Increment ] [ text "+" ]
        ]


type Msg
    = Increment
    | Decrement


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



{-
   Elm architecture essentially boils down to these three parts: model, view and update. The entire application can be viewed
   as one giant machine that runs in perpetuity. The Browser.sandbox function describes the structure of that machine: take
   an initial model, present it to the user, listen to messages, update the model based on those messages, and present the
   updated model back to the user again.

                   render                   message
           Model --------------->  View  --------------> Update ---
           ^                                                       |
           |                                                       |
           ---------------------------------------------------------


    # How browsers render HTML

    When a Browser is given some HTML code, it takes that code through a series of transformations before the UI elements
    are displayed on a screen.

        Parse HTML --------> Construct DOM Tree ----------> Construct Render Tree -------------> Layout Render Tree
                                                                                                        |
                                                                                                        |
                                                                                                        |
                                                                                                        V
                                                                                             Paint render tree on Screen


    1. Parse HTML
    The first thing a browser does when it encounters the HTML is to create HTML tokens. Browsers use a process called
    tokenization or lexical analysis to break a chunk of HTML code into individual tokens.

    2. Construct the DOM Tree
    During the tokenization phase, the browser processed our code, which started as a stream of text characters, into a
    series of tokens. The next step is to create the Document Object Model (DOM) tree using these tokens.

        The Document Object Model (DOM) is a programming interface for HTML and XML documents. It represents the page so
        that programs can change the document structure, style and content. The DOM represents the document as nodes and
        objects.

    After an HTML page is loaded, we often need to make changes to it based on various events generated either by the user
    or some other source. To make this update easier, the browser creates the DOM tree for the page. With access to the
    object representation of each element in the HTML page, we can now add, change, and remove HTML elements and attributes;
    change the CSS styles; react to existing events; or create new events through JavaScript.

    The objects in the DOM tree are often referred to as `node`s in HTML parlance. The root node in the DOM tree is known
    as `document`. Nodes that can be used to specify an HTML tag are called `elements`. The element attributes such as id and
    class can be represented using the `attribute` nodes. Similarly the comments and plain text can be represented using
    the `comment` and `text` nodes respectively.


    # Construct the Render Tree
    While the DOM tree is being created, the browser starts the rendering process in parallel by creating yet another tree
    called the render tree. The render tree contains the style information for each DOM node that dictates how the node
    should be displayed. Nodes that aren't meant to be displayed on a screen aren't included in the render tree. Here are
    a few examples of nodes that aren't visible: `head`, `script` and `meta`. Some nodes might be hidden via CSS through
    the use of `display:none` even though they can actually be displayed on a screen. These nodes are also excluded from
    the render tree.

    # Layout the Render Tree
    With the render tree in place, the browser proceeds to layout each node. The render tree contains the information that
    dictates how the nodes should be displayed, but not where on a screen they should be rendered. The browser computes
    the position and size of each node from the information stored in the render tree.

    # Paint the Render Tree on the Screen
    Now that all information required to render the nodes has been computed, the browser begins to convert each node in
    the render tree to actual pixels on the screen. This process is called painting.


-}
{-
   # Virtual DOM
    Although the DOM tree makes it easy for us to create, update and delete nodes, the direct manipulation of that tree
    tends to be really slow when we have large number of nodes.

    Updating a string in Javascript is straightforward. JavaScript knows where the string is stored in memory, so it takes
    the new value and simply overwrites the bits in memory. But when we update a DOM node, it's not JavaScript that's
    doing the work. The browser needs to take the new change through the entire process laid out earlier:
        * Parse HTML
        * Update the DOM tree
        * Update the Render tree
        * Layout the Render tree
        * Paint the render tree on the screen.

    Depending on how complex the change is, the browser may need to destroy and re-create the existing children nodes of
    the element we're trying to update. It may also need to recompute the physical dimensions and style of many nodes that
    are affected by the change. If there are any browser extensions that control access to the node we want to modify, the
    browser will have to first ask for their permission and notify them after the change has been made.

    Elm mitigates this problem by making as few updates as possible to the DOM tree. Elm actually takes over the entire
    responsibility for updating the DOM tree from us.

    Algorithm for Rendering HTML Elements in Elm

    1. Create a virtual DOM tree. This is not an exact replica of the real DOM tree. It's just a light-weight version
    built using JavaScript Objects.
    2. Make a UI change in our app. This change could be anything, for example changing the color of a button, or adding
    a completely new UI element, or removing an existing one.
    3. Create a new virtual DOM tree that reflects the UI changes we made. This tree is different from the one created in
    Step 1.
    4. Figure out the exact differences between the new and previous virtual DOM trees. This step is where the virtual DOM
    really shines. If we were to compute the difference between the new and previous UI using the real DOM, we would take
    a huge performance hit. In contrast, the virtual DOM is highly optimized for operations like these.
    5. Repeat step 2 through 4 until all UI changes have been identified.
    6. Create instructions for modifying the real DOM tree in one big batch. Even a seemingly small change to the real DOM
    tree is expensive from performance standpoint. So it's prudent to combine multiple instructions together rather than
    applying each change individually.
    7. Modify the real DOM tree using the `requestAnimationFrame` function which figures out the best way to apply multiple
    changes so that the transition from the previous UI to the new one is smooth.
-}
