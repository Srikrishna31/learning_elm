port module Ports exposing (..)

{-
   Data Exchange with the Browser and JS Code

   An Elm program exists within the browser environment, and usually needs to interact with the browser functionality or
   supporting JavaScript code. For this reason, Elm provides facilities to interface with the outside world. Ports and
   Subscriptions are language level mechanism for exchanging data with the Browser and JavaScript.

   Ports
   Ports work in tandem with commands and subscriptions. They define what data your program receives from its environment
   and what data it sends out.
   From the point of view of Elm code, a port for outgoing data is defined as a function taking some data and returning
   a command. This function can then be used in update to generate a command. A port for incoming data is defined as a
   function returning a Sub value which is the type for subscriptions.

   Subscriptions
   Subscriptions are a mechanism for receiving data from the outside environment, which for an Elm program consists of the
   browser and possibly JavaScript code. This means things like time, key presses, position updates, and data from JavaScript.
   A point to keep in mind is that both subscriptions and commands are descriptions of what you want to happen - in other
   words, they are data rather than actions. It's up to the Elm runtime to take these descriptions and use them as
   instructions to perform actions. Based on the subscriptions, the runtime generates messages which flow back into the
   update function.

   Flags
   An application can receive data at the start of execution, and incorporate it into its initial state. This data is
   referred to as "flags" in Elm as it's similar to passing some flags into a program on commandline.

   Data validation and conversion
   In the case of both incoming ports and flags, the data is validated to ensure that your Elm code doesn't receive something
   unexpected. If the data fails validation, a runtime exception is thrown. Both incoming and outgoing ports allow all
   valid JSON values to be passed.
   Some type conversions happen when passing data through ports:

    * Booleans, strings, integers and floating point numbers convert trivially
    * Maybe values: a Nothing appears as null on the JavaScript side, and a Just 1 appears as 1.
    * Both lists and arrays on the Elm side correspond to JavaScript arrays.
    * Records correspond to JavaScript objects.
    * Tuples correspond to fixed-length, mixed-type JavaScript arrays
    * Values of type Json.Encode.Value correspond to arbitrary JSON.
-}


port saveSessionId : Maybe String -> Cmd msg



{-
   The incoming port must be a function that takes one argument and returns a Sub msg. The argument is itself a function
   that takes a single argument which defines the type of data sent through the port, and returns msg.
-}


port dumpModel : (() -> msg) -> Sub msg
