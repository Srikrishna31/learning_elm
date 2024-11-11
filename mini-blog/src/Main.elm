module Main exposing (main)

import AboutPage
import Browser exposing (Document, UrlRequest)
import Browser.Navigation
import Element exposing (Element)
import Element.Font
import HomePage
import Html
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


type alias Model =
    { title : String
    , url : Url.Url
    , navigationKey : Browser.Navigation.Key
    }


type Msg
    = MsgDummy
    | MsgUrlChanged Url.Url
    | MsgUrlRequested Browser.UrlRequest


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ url navigationKey =
    ( initModel url navigationKey, Cmd.none )


initModel : Url.Url -> Browser.Navigation.Key -> Model
initModel url navigationKey =
    { title = "Hello Navigation"
    , url = url
    , navigationKey = navigationKey
    }


view : Model -> Document msg
view model =
    { title = getTitle model.url
    , body = [ viewContent model ]
    }


viewContent : Model -> Html.Html msg
viewContent model =
    Element.layout []
        (Element.column [ Element.padding 22 ]
            [ viewLink "/about" "About"
            , viewLink "/" "Home"
            , viewPage model
            , viewLink "https://www.duckduckgo.com" "DuckDuckGo"
            , viewLink "https://www.ecosia.org" "Ecosia"
            ]
        )


getTitle : Url.Url -> String
getTitle url =
    if String.startsWith "/about" url.path then
        "About"

    else
        "My blog"


viewPage model =
    if String.startsWith "/about" model.url.path then
        AboutPage.view

    else
        HomePage.view


viewLink : String -> String -> Element msg
viewLink url caption =
    Element.link
        [ Element.Font.color (Element.rgb255 0x11 0x55 0xFF)
        , Element.Font.underline
        , Element.Font.size 12
        ]
        { url = url
        , label = Element.text caption
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MsgDummy ->
            ( model, Cmd.none )

        MsgUrlChanged url ->
            ( { model | url = url }, Cmd.none )

        MsgUrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( { model | title = url }, Browser.Navigation.load url )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


onUrlChange : Url.Url -> Msg
onUrlChange url =
    MsgUrlChanged url


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    MsgUrlRequested urlRequest
