module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Post exposing (PostId)
import Url exposing (Url)
import Url.Parser exposing (..)



{-
   # The Url Type

    The Url module defines a type alias by the same name:

    type alias Url =
        { protocol: Protocol
        , host: String
        , port_: Maybe String
        , path: String
        , query: Maybe String
        , fragment: Maybe String
        }

    Here's the definition for Protocol type:

    type Protocol
        = Http
        | Https
-}


type Route
    = NotFound
    | Posts
    | Post PostId
    | NewPost



{-
   The parseUrl function uses parse - defined in the Url.Parser module - to extract a path from the given url and translate
   it to one of the values in the Route type. The parse function, in turn uses `matchRoute` to check if the given url
   contains one of these routes:

    * Posts - Represents the `ListPosts` page
    * NotFound - represents the not-found page
-}


parseUrl : Url -> Route
parseUrl url =
    Maybe.withDefault NotFound <| parse matchRoute url



--case parse matchRoute url of
--    Just route ->
--        route
--
--    Nothing ->
--        NotFound
{-
   Matching Routes

    `matchRoute` defines parsers that know how to extract a given path from a url.

    top defines a parser that doesn't look for any path
    The function s also defines a parser, but it takes a path as an argument.
    The oneOf function executes the parsers one at a time starting from the top. It stops as soon as a match is found for
    the entire path and not just a portion of it.

    The parser for matching an individual post route uses </> to combine two different parsers. This </> operator combines
    the parsers created by s and Post.idParser and creates a new parser that knows how to match a path for an individual
    post.
-}


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Posts top
        , map Posts <| s "posts"
        , map NewPost <| s "posts" </> s "new"

        -- Since the id is typed as a string, placing the Post.idParser line above new posts will route to edit posts page.
        , map Post <| s "posts" </> Post.idParser
        ]



{-
   # Routing to the correct page

   The overall process for routing users to the correct page can be summarized in the following steps:

   1. Enter a full url in the browser's address bar.
   2. Convert the url from step 1 to Url type. This is done by Elm runtime behind the scenes.
   3. Extract a route from url and store it in the route field inside Main module'e model
   4. Determine which page to display based on route.
   5. Ask the page from step 4 to return its model by calling its init function.
   6. Pass the model from step 5 to that page's view function.
-}


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Posts ->
            "/posts"

        Post postId ->
            "/posts/" ++ Post.idToString postId

        NewPost ->
            "/posts/new"
