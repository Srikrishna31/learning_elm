module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes as Html exposing (..)
import Html.Events exposing (onClick)
import Random



{-
   The `Browser.sandbox` function takes a record with three fields:
       * model - A value that can be anything you please
       * view - A function that takes a model and returns an Html node
       * update - A function that takes a message and a model, and return a new model
-}
{-
   The () value is known as unit. It contains no information whatsoever. It's both a value
   and a type; the () type can be satisfied only with the () value. An example:

    getUltimateAnswer: () -> Int
    getUltimateAnswer unit = 40 + 2

    The only way to call getUltimateAnswer would be to pass () to it; because it accepts
    () as an argument, no other value but () will do.

    Putting it together, we can read Program () Model Msg as "an Elm program with no flags, whose
    model type is Model and whose message type is Msg".
-}


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


photoListUrl : String
photoListUrl =
    "https://elm-in-action.com/list-photos"


urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , Html.classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSizde size =
    label []
        {-
           The underscore in the type below, type_ attribute is very important.
           type is a reserved keyword in Elm, so the Html.Attributes module names
           the attribute type_ to work around this.
        -}
        [ input [ type_ "radio", name "size", value (sizeToString size), checked (chosenSizde == size), onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }



-- A type alias assigns a name to a type. Anywhere you would refer to that type, you can substitute this name instead.


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | GotSelectedIndex Int
    | ClickedSurpriseMe


type ThumbnailSize
    = Small
    | Medium
    | Large


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



{-
   Because Elm forbids functions from returning different values when they receive
   the same arguments, it's not possible to write an inconsistent function like Math.random
   as a plain Elm function. Instead, Elm implements random number generation by using a command.

    A command is a value that describes an operation for Elm runtime to perform. Unlike calling
    a function, running the same command multiple times can have a different result.
-}


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)



{-
   In general, when update function receives a message, it will do the following:
   1. Look at the message  it received
   2. Look at our current model
   3. Use these two values to determine a new model, and then return it.

   Because update returns a tuple of a new Model and a command, it's possible for the
   same update branch to change the Model and to run a command as well. In such a case,
   first Model would change, then view would get run on the new Model to update the page,
   and finally the command would run.
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        {-
           We could write the type annotation for Random.generate like so:
           generate : (randomValue -> msg) -> Random.Generator randomValue -> Cmd msg

           Capitalization is important. There is a big difference between Msg and msg here.
           A function that returns Msg returns an instance of exact Msg custom type defined above.
           In contrast, msg is a type variable. A function that returns msg could return anything
           at all.
        -}
        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )



{-
   1. A model represents our application state.
   2. A view function takes a model and returns a list of Html nodes.
   3. User events such as clicks get translated into message values.
   4. Messages get run through the update function to produce a new model.
   5. After an update, the new model is sent to the view function to determine the new DOM.
   6. Browser.sandbox wires together model, view and update.
-}
