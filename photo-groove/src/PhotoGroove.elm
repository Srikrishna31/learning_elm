module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Html exposing (..)
import Html.Events exposing (onClick)



{-
   The `Browser.sandbox` function takes a record with three fields:
       * model - A value that can be anything you please
       * view - A function that takes a model and returns an Html node
       * update - A function that takes a message and a model, and return a new model
-}


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
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
        , div [ id "thumbnails" ]
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
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }



-- A type alias assigns a name to a type. Anywhere you would refer to that type, you can substitute this name instead.


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }


type alias Msg =
    { description : String, data : String }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



{-
   In general, when update function receives a message, it will do the following:
   1. Look at the message  it received
   2. Look at our current model
   3. Use these two values to determine a new model, and then return it.
-}


update : { description : String, data : String } -> Model -> Model
update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }

    else
        model



{-
   1. A model represents our application state.
   2. A view function takes a model and returns a list of Html nodes.
   3. User events such as clicks get translated into message values.
   4. Messages get run through the update function to produce a new model.
   5. After an update, the new model is sent to the view function to determine the new DOM.
   6. Browser.sandbox wires together model, view and update.
-}
