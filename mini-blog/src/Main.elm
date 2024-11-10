module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation
import Element
import Element.Font
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
    }


type Msg
    = MsgDummy
    | MsgUrlChanged Url.Url
    | MsgUrlRequested Browser.UrlRequest


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ url navigationKey =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { title = "Hello Navigation"
    }


view : Model -> Document msg
view model =
    { title = "Test"
    , body = [ viewContent model ]
    }


viewContent : Model -> Html.Html msg
viewContent model =
    Element.layout []
        (Element.column [ Element.padding 22 ]
            [ Element.text model.title
            , Element.link
                [ Element.Font.color (Element.rgb255 0x11 0x55 0xFF)
                , Element.Font.underline
                ]
                { url = "https://www.duckduckgo.com"
                , label = Element.text "DuckDuckGo"
                }
            ]
        )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MsgDummy ->
            ( model, Cmd.none )

        MsgUrlChanged url ->
            ( model, Cmd.none )

        MsgUrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Cmd.none )

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
