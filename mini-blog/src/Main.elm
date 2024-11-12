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
    { url : Url.Url
    , navigationKey : Browser.Navigation.Key
    , modelAboutPage : AboutPage.Model
    , modelHomePage : HomePage.Model
    }


type Msg
    = MsgUrlChanged Url.Url
    | MsgUrlRequested Browser.UrlRequest
    | MsgAboutPage AboutPage.Msg
    | MsgHomePage HomePage.Msg


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ url navigationKey =
    ( initModel url navigationKey, Cmd.none )


initModel : Url.Url -> Browser.Navigation.Key -> Model
initModel url navigationKey =
    { url = url
    , navigationKey = navigationKey
    , modelAboutPage = AboutPage.initModel
    , modelHomePage = HomePage.initModel
    }


view : Model -> Document Msg
view model =
    { title = getTitle model.url
    , body = [ viewContent model ]
    }


viewContent : Model -> Html.Html Msg
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


viewPage : Model -> Element.Element Msg
viewPage model =
    if String.startsWith "/about" model.url.path then
        Element.map MsgAboutPage (AboutPage.view model.modelAboutPage)

    else
        Element.map MsgHomePage (HomePage.view model.modelHomePage)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgUrlChanged url ->
            ( { model | url = url }, Cmd.none )

        MsgUrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url.path == "/about/hide-detail" then
                        let
                            hiddenDetailModel =
                                { showDetail = False
                                }
                        in
                        ( { model | modelAboutPage = hiddenDetailModel }, Cmd.none )

                    else
                        ( model, Browser.Navigation.pushUrl model.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        MsgAboutPage msgAboutPage ->
            let
                ( newAboutPageModel, cmdAboutPage ) =
                    AboutPage.update msgAboutPage model.modelAboutPage
            in
            ( { model | modelAboutPage = newAboutPageModel }, Cmd.map MsgAboutPage cmdAboutPage )

        MsgHomePage msgHomePage ->
            let
                ( newHomePageModel, cmdHomePage ) =
                    HomePage.update msgHomePage model.modelHomePage
            in
            ( { model | modelHomePage = newHomePageModel }, Cmd.map MsgHomePage cmdHomePage )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MsgAboutPage
            (AboutPage.subscriptions model.modelAboutPage)
        , Sub.map MsgHomePage
            (HomePage.subscriptions model.modelHomePage)
        ]


onUrlChange : Url.Url -> Msg
onUrlChange url =
    MsgUrlChanged url


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    MsgUrlRequested urlRequest
