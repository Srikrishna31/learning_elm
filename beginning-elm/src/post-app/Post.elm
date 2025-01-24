module Post exposing (Post, PostId, emptyPost, idParser, idToString, newPostEncoder, postDecoder, postEncoder, postsDecoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)



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
    Decode.map PostId string


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


type PostId
    = PostId String


idToString : PostId -> String
idToString (PostId id) =
    -- use pattern matching to expose the underlying integer value inside the PostId data constructor.
    id



{-
   # Primitive Parsers
   The `Url.parser` module defines three primitive parsers as shown below:

       int : Parser (Int -> a) a
       string: Parser (String -> a) a
       s : String -> Parser a a

   To understand how these parsers work, let's imagine a type called FakeRoute as defined below:

       type FakeRoute
           = Home
           | Posts
           | Post PostId
           | User Username
           | Comment Username CommentId

       type PostId
           = PostId Int

       type Username
           = Username String

       type CommentId
           = CommentId Int

    Below are the parsers for matching routes listed in FakeRoute:

       matchFakeRoute: Parser (Route -> a) a
       matchFakeRoute =
           oneOf
               [ map Home top
               , map Posts (s "posts")
               , map Post (s "posts" </> int)
               , map User (s "user" </> string)
               , map Comment (s "user" </> string </> s "comment" </> int)
               ]

     The table below shows which path gets parsed to which route based on the logic in `matchFakeRoute`

     Path                           Parser                                                      Route
                                    top                                                         Home
     /posts                         s "posts"                                                   Posts
     /posts/1                       s "posts" </> int                                           Post 1
     /user/pam                      s "user" </> string                                         User "pam"
     /user/pam/comment/12           s "user" </> string </> s "comment" </> int                 Comment "pam" 12


     Both `string` and `int` parsers pluck values out of a path, whereas `s` simply matches the given string. So when we
     use s "posts" </> int to parse /posts/1, the s parser first verifies that the path indeed starts with posts. After
     that the int parser comes in and extracts 1 from the path.

     Now that the path has been parsed successfully, we need to map the result to the Post data constructor from FakeRoute.
     It's important to note that `s` expects the path segment to match exactly. Therefore if the given path is /postss/1
     it'll fail.

     # Custom Parsers
     Primitive parsers are only capable of converting a path segment to either `String` or `Int`. If we need to convert
     a segment to any other type, we must create our own parser using the `custom` function from `Url.Parser`.

        custom: String -> (String -> Maybe a) -> Parser (a -> b) b

     (String -> Maybe a) function is given a path segment as an input. In our app, that segment is a post ID in string
     format.
-}


idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <|
        --\postid -> String.toInt postid |> Maybe.map PostId
        \postid -> Just (PostId postid)


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId id) =
    Encode.string id


emptyPost : Post
emptyPost =
    { id = PostId ""
    , title = ""
    , authorName = ""
    , authorUrl = ""
    }


newPostEncoder : Post -> Encode.Value
newPostEncoder post =
    Encode.object
        [ ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]
