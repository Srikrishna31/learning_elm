{-
   Port modules expose public interfaces(ports) for JavaScript code to interact with an Elm application.
-}


port module Websocket exposing (listen, receive)

{-
   A WebSocket is a network protocol that allows a client application and a server to communicate back and forth.
   WebSockets are great for applications, such as chat apps, that require realtime notifications. WebSockets allow a server
   to effectively push data to a client, perfect for updating a photo feed in real time.
-}


port listen : String -> Cmd msg


port receive : (String -> msg) -> Sub msg
