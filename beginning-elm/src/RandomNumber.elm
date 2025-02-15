module RandomNumber exposing (..)

{-
   # Generating Random Numbers

   There are two main approaches to generating random numbers: True Random Number Generators (TRNGs) and
   Pseudo-Random Number Generators (PRNGs). TRNGs generate numbers from truly random physical phenomena, for example the
   little variations in someone's mouse movements or the point in time at which a radioactive material decays or the
   atmospheric noise picked up by a radio.

   Due to their reliance on a physical phenomena, TRNGs take considerably longer time to generate random numbers. Therefore,
   most computer programs that need random numbers quickly tend to rely on PRNGs which are much more efficient than TRNGs.
   Despite their efficiency, PRNGs are not suitable for applications that need truly unpredictable random numbers, such
   as encryption key generators.

   PRNGs take an initial value (called seed) and apply an algorithm to generate a seemingly random number.

   # Random.step
   Random.step function allows us to generate a random value. The output is a tuple. The first element is a random number
   and the second element is the seed we can use to generate the next random number.

    step: Generator a -> Seed -> (a, Seed)
-}

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GenerateRandomNumber ] [ text "Generate Random Number" ]
        , text <| String.fromInt model
        ]


type Msg
    = GenerateRandomNumber
    | NewRandomNumber Int



{-
   When the GenerateRandomNumber message is received, we return an unmodified model and a command for generating a random
   number. The command is generated by using the `generate` function defined in the `Random`  module.

   `generate` tries to achieve the same goal as the step function - generate a random number. It takes a generator and
   tells the Elm runtime to run that generator. It also tells the runtime which message to send when a number is ready.
   Here's what the generate function's type signature looks like:

    generate: (a -> msg) -> Generator a -> Cmd msg

    The first argument is a function that takes a value and wraps it in a message. The second argument to generate is a
    random number generator. Lastly, generate returns a command that encapsulates the message and the generator.
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomNumber ->
            ( model, Random.generate NewRandomNumber <| Random.int 0 100 )

        NewRandomNumber number ->
            ( number, Cmd.none )



{-
   To run things that generate side effects, Elm requires us to create a command along with any pertinent information
   that command needs. We then hand that command over to the runtime. The runtime executes the command and notifies our
   application with the result by sending the message included in the command.
-}
{-
   Browser.sandbox vs Browser.element
   Browser.sandbox creates an Elm program that cannot communicate with the outside world by issuing commands.
       update: Msg -> Model -> Model

   In contrast, Elm programs created with Browser.element are able to communicate with the outside world.
       update: Msg -> Model -> (Model, Cmd Msg)

   The init field in Browser.element expects a function that takes flags and returns a tuple containing model and commands.

    `sub
-}


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
