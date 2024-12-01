port module PhotoGroove exposing (FilterOptions, main, setFilters)

import Browser
import Html exposing (Attribute, Html, button, canvas, div, h1, h3, img, input, label, node, text)
import Html.Attributes as Attr exposing (checked, class, id, name, src, title, type_, value)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JEncode
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
        { init = \_ -> ( initialModel, initialCmd )
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



{-
   Optimized DOM Updates
       One reason the Elm Runtime has good performance is that it skips unnecessary renders. See, browsers repaint the
       DOM as pixels on users' screen only every so often. If the Elm Runtime changes part of the DOM, and then changes
       it again before the next repaint, the first change will have been wasted time; only the second change will be
       painted for the users to see.
       Calling update function a million times in a single second doesn't necessarily call the view function the same
       number of times. Although those million updates will result in a million potential Model changes, Elm waits until
       the browser's next repaint to call view even once--with whatever value Model has at that moment. Invoking view
       more frequently than that would result in DOM updates that the browser wouldn't bother to paint anyway.
-}


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (JEncode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas
        [ id "main-canvas"
        , class "large"

        --, src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , Attr.classList [ ( "selected", selectedUrl == thumb.url ) ]
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


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }



-- A type alias assigns a name to a type. Anywhere you would refer to that type, you can substitute this name instead.
{-
   1. A model represents our application state.
   2. A view function takes a model and returns a list of Html nodes.
   3. User events such as clicks get translated into message values.
   4. Messages get run through the update function to produce a new model.
   5. After an update, the new model is sent to the view function to determine the new DOM.
   6. Browser.sandbox wires together model, view and update.
-}


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String



{-
   Because Elm forbids functions from returning different values when they receive
   the same arguments, it's not possible to write an inconsistent function like Math.random
   as a plain Elm function. Instead, Elm implements random number generation by using a command.

    A command is a value that describes an operation for Elm runtime to perform. Unlike calling
    a function, running the same command multiple times can have a different result.

    Random.uniform : elem -> List elem -> Random.Generator elem

    Conceptually, Random.uniform "takes a non-empty list." The reason Elm's standard libraries don't
    include a dedicated NonEmptyList type is that it's simple enough for a function that needs one
    to follow Random.uniform's design: accept a mandatory elem as well as a List elem of optional
    additional values. Similary, a function can "return a non-empty list" by returning an
    (elem, List elem) tuple.

    All side-effects are performed by the Elm Runtime itself; Elm code only describes which effects to
    perform, by returning values from update.
    Note: Calling update does not directly alter any state. All update does is return a tuple. If you wanted to,
    you could call update a hundred times in a row and all it would do is give you back a hundred tuples.

    This system of "managed effects" in which the Elm runtime is in charge of performing all effects, means that
    Elm programs can be written entirely in terms of data transformations.

-}
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
            applyFilters { model | status = selectUrl url model.status }

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
            case model.status of
                {-
                   The piplined code is saying the following:
                   1. Call Random.uniform firstPhoto otherPhotos
                   2. Pass its return value as the final argument to Random.generate GotRandomPhoto
                   3. Pass that return value as the final argument to Tuple.pair model.
                -}
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

                Loaded [] _ ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    {-
                       Using type aliases to create records
                            Declaring type alias Photo = {url:String} does more than give us a Photo type we
                            can use in type annotations. It also gives us convenience function whose job it is
                            to build Photo record instances. This function is also called Photo.
                            This also works with type aliases involving multiple fields, like the one for Model:
                            type alias Model =
                            { status: Status
                            , chosenSize: ThumbnailSize
                            }
                            This declaration gives us a convenience function called Model that builds a record
                            and returns it:
                            Model : Status -> ThumbnailSize -> Model
                            The order of arguments matches the order of the fields in the type alias declaration.
                            So, if you were to move the photos: List Photo declaration to the end of the
                            type alias, then the Model function would look like this instead:
                            Model: String -> ThumbnailSize -> List Photo -> Model

                            In the code below, we replace the lambda \url -> {url = url} with Photo!
                            -- The below code
                            has been refactored out, but the comment remains for educational purposes.
                    -}
                    applyFilters
                        { model
                            | status =
                                case List.head photos of
                                    Just photo ->
                                        Loaded photos photo.url

                                    Nothing ->
                                        Loaded [] ""
                        }

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        SlidHue int ->
            applyFilters { model | hue = int }

        SlidRipple int ->
            applyFilters { model | ripple = int }

        SlidNoise int ->
            applyFilters { model | noise = int }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loading ->
            ( model, Cmd.none )

        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Errored string ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        --thought
        Errored errorMessage ->
            status



{-
   Http.Get
       Calls to Http.get can result in different results, even though they are passed the same parameters.
       This is because of underlying network issues or server issues. Hence, Http.Get cannot be a pure Elm function.
       Instead, it returns a Cmd which is executed by the Elm runtime, which manages the effects, and returns a uniform
       result back to us.
      The Http.get function returns a Cmd representing the HTTP request we want to make:
       Http.get :{url: String, expect: Expect msg} -> Cmd msg
       We pass Http.get a URL string, along with an Expect value that describes what we expect
       to get back. Once this Cmd completes, it will send a Msg to update telling us what happened.
   Suppose we called Http.get as below:
       Http.get { url = "https://www.manning.com"
                , expect = Http.expectString toMsg
                }
       The above code means the following steps:
       1.Send an Http GET request to https://www.manning.com
       2.I expect to get back a String for the response.
       3.When the response comes back, use this toMsg function to translate it into an Msg.
       4.Send that Msg to update.
-}


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "https://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder) -- can also be (\result -> GotPhotos result)
        }



{-
   Because of type aliasing, Photo function is equivalent to below definition of buildPhoto:
   buildPhoto : String -> Int -> String -> Photo
   buildPhoto url size title =
       Photo url size title
-}


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"



{-
   Decoding JSON HTTP responses
    The Http.expectJson function requests data from a server and then decodes it. Here's how the
    types of Http.expectJson and Http.expectString match up:

    expectString: (Result Http.Error String -> msg)           -> Expect msg
    expectJson: (Result Http.Error val -> msg) -> Decoder val -> Expect msg

    Comparing types like this suggests how these functions are similar and how they differ. They
    both accept a function to translate a Result into a msg. Both Result types have Http.Error
    as their Error type. Howeve, wherease expectString takes no other arguments and always produces
    a String as its Result's Ok type, expectJson additionally accepts a Decoder val, and on success
    produces an Ok val result instead of Ok String.
-}


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children



{-
   Html.Events.on
   The Html.Event.on lets us create a custom event handle, just as the Html.node function lets us create a custom element
   and the Html.Attributes.property function lets us create a custom property.
    The CustomEvent object JavaScript will dispatch for "slide" is shaped something like this:
    {detail: {userSlidTo: 7}}
   The detail field is what CustomEvent objects use to hold their custom information, which in our case holds a single
   field we named userSlidTo.
   We can use the Json.Decode.field and Json.Decod.int functions to write a decoder like so:

    field "detail" (field "userSlidTo" int)

    Json.Decode.at
    There's a convenience function in Json.Decode for the case where we want to call field on another field like this:
    Json.Decode.at. It takes a list of field strings and traverses them in order. These two decoders do the same thing:

    field "detail" (field "userSlidTo" int)
    at ["detail", "userSlidTo"] int
-}


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    {-
       on "slide" (Json.Decode.map toMsg (at ["detail", "userSlidTo"] int))

       The above code can be written in pipeline style as below
    -}
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"



{-
   Talking to JavaScript is like talking to Servers
   Because calling any JavaScript function may result in a side effect, Elm functions cannot call JavaScript functions
   anytime they please; this would destroy the guarantee that Elm functions have no side effects.
   Instead, Elm talks to JavaScript the same way it talks to servers: by sending data out through a command, and
   receiving data in through a message. This means that talking to Javascript will have some characteristics in common:
    * Data can be sent only by using a command.
    * Data can be received only by update, and that data must be wrapped in a message.
    * We can translate and validate this incoming data by using decoders.

   Note: In JavaScript, some effects are performed synchronously, with program execution halting until the effect completes.
   In contrast, an Elm Cmd always represents an asynchronous effect. This mean that when we send data to Javascript, it's
   always possible that other JavaScript code might run before data gets sent back to Elm.
-}


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }



{-
   Using a port to define a function
   We don't write an implementation for port functions, because, the port keyword automatically writes one for us. port only
   needs to look at the type we requested to decide what the function should do.

   All port functions that send data to JavaScript are defined using a very specific pattern:
       * The port keyword must be followed by a function name and a type annotation
       * The type annotation must be for a function that takes one argument.
       * The function must return Cmd msg, and nothing else-not even Cmd Msg!

   Port commands never send messages:

   The official documentation for Cmd.none shows that it has this type:

   none: Cmd msg

   A Cmd msg by itself like this is a command that produces no message after it completes.

   A command that produces no messages has the type Cmd msg, a subscription (for example, Sub.none) that produces no
   messages has the type Sub msg, and a list that has no elements--that is, [] --has the similar type List val. Because
   their type variables have no restriction, you can use a Cmd msg anywhere you need any flavor of Cmd, just as you can
   use an empty list anywhere you need any flavor of List.

   Both Cmd.none and setFilters produce no message after completing. The difference is that Cmd.none has no effect, whereas
   setFilters will perform the effect of sending data to JavaScript. (Specifically, it will send the FilterOptions value
   we pass it). We can think of setFilters as a "fire and forget" command.

   Although HTTP requests can fail, sending data to JavaScript cannot. We don't miss out on any error-handling opportunities
   just because setFilters sends no messages back to update.
-}


port setFilters : FilterOptions -> Cmd msg
