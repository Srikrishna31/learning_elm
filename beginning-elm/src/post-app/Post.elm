module Post exposing (Post, PostId, idToString, postDecoder, postsDecoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



{-
       Single-Page App

   On a very high level, web applications tend to have two major parts: front end and back end. The back end is responsible
   for computing data and the front end is responsible for presenting that data to the users.

   The front end primarily consists of a presentation layer for displaying and navigating between HTML pages. Whereas
   the back end consists of a data layer for retrieving data from a database or an external service and a business
   layer for performing operations on that data.

   # Traditional Web Applications

   In a traditional web application, both front and back ends are implemented on the server-side. A request is sent from
   a browser to a server. The presentation layer intercepts the request and forwards it to a component in business
   layer responsible for fulfilling that request. If the business component needs to access data from a database or an
   external service, it'll make that request to the data layer. The business layer returns the result of its computation
   to the presentation layer. The presentation layer then determines how to display that result and sends an HTML page
   back to the browser.

   # Single-Page Web Applications

   Often times, after an application is loaded in a browser, all client-side wants to do is send or receive new data from
   a server. However, with traditional web application architecture, the server has to send an entire HTML page back and
   not just the fresh data client is interested in. This is because the presentation layer sits on the server side.
   Without this layer, the client is just a dumb terminal for requesting pages.

   With the Single-page architecture, the entire front end is loaded at once on the client-side. This enables the client
   to send and receive just the data it's interested in and nothing more. It can send those requests asynchronously via
   ajax.

   Instead of receiving a full HTML page as a response, the client now gets a JSON representation of the data it requested.
   (Although a response typically is in JSON format, it doesn't have to be.) It then updates parts of the page that
   display the new data without having to reload the entire page. This approach reduces unnecessary overhead incurred by
   traditional web applications. This is how most single-page applications (SPAs) work.


   # Application Shell
   The term single-page means that it's a shell that defines an overall layout of the application. This layout divides
   the application into multiple regions, for example header, footer, sidebar and main content.

   Whenever the user attempts to navigate from one part of the application to another, the application determines which
   region(s) needs to be refreshed. It will then swap the view that is currently being displayed in that region with a
   new view. During the initial load, the client side fetches all views in the application along with the shell. It has
   to do that because in an SPA architecture, the entire presentation is moved to the client side.

   Of course the application doesn't display all of those views at the same time. It will wait for the right conditions
   to be met before showing a particular view. This mechanism is what enables SPAs to present rich user interfaces
   without having to retrieve a new page from the server every time the user attempts to navigate to a different part of
   the application.
-}


type alias Post =
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder


idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


type PostId
    = PostId Int


idToString : PostId -> String
idToString (PostId id) =
    -- use pattern matching to expose the underlying integer value inside the PostId data constructor.
    String.fromInt id
