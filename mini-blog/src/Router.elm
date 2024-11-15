module Router exposing (..)

import Url
import Url.Parser exposing ((</>))


type Route
    = RouteAboutPage
    | RouteHomePage


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Url.Parser.parse routerParser url


asPath : Route -> String
asPath route =
    case route of
        RouteAboutPage ->
            "/about"

        RouteHomePage ->
            "/"


aboutPageParser : Url.Parser.Parser a a
aboutPageParser =
    Url.Parser.s "about"


homePageParser : Url.Parser.Parser a a
homePageParser =
    Url.Parser.top


routerParser : Url.Parser.Parser (Route -> b) b
routerParser =
    Url.Parser.oneOf
        [ Url.Parser.map RouteAboutPage aboutPageParser
        , Url.Parser.map RouteHomePage homePageParser
        ]


referenceContactPageParser : Url.Parser.Parser a a
referenceContactPageParser =
    Url.Parser.s "about" </> Url.Parser.s "contact"
