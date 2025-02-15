module Main exposing (main)

import Browser
import Browser.Events
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html exposing (Html)
import Http exposing (Error(..))
import Json.Decode as JD
import Svg as S
import Svg.Attributes as SA


type alias Model =
    { searchText : String
    , results : List Book
    , errorMessage : Maybe String
    , loading : Bool
    }


type alias Book =
    { title : Maybe String
    , thumbnail : Maybe String
    , link : String
    , pages : Maybe Int
    , publisher : Maybe String
    }


type Msg
    = MsgSearch
    | MsgGotResults (Result Http.Error (List Book))
    | MsgInputTextField String
    | MsgKeyPressed String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, cmdSearch initModel )


initModel : Model
initModel =
    { searchText = "Elm Language"
    , results = []
    , errorMessage = Nothing
    , loading = False
    }


view : Model -> Html.Html Msg
view model =
    viewLayout model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgInputTextField newTextInput ->
            ( { model | searchText = newTextInput }, Cmd.none )

        MsgSearch ->
            updateStartSearch model

        MsgKeyPressed key ->
            if key == "Enter" then
                updateStartSearch model

            else
                ( model, Cmd.none )

        MsgGotResults result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | results = data }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl string ->
                                    string

                                Timeout ->
                                    "Request Timed out"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody string ->
                                    string
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )


updateStartSearch : Model -> ( Model, Cmd Msg )
updateStartSearch model =
    ( { model | loading = True }, cmdSearch model )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress keyPressed


keyPressed : JD.Decoder Msg
keyPressed =
    JD.map MsgKeyPressed (JD.field "key" JD.string)


viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Just (E.rgb255 0x00 0x33 0x66)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (E.column [ E.padding 20 ] [ viewSearchBar model, viewErrorMessage model, viewResults model ])


loadingImage : Html.Html msg
loadingImage =
    S.svg
        [ SA.width "64px"
        , SA.height "64px"
        , SA.viewBox "0 0 48 48"
        ]
        [ S.circle
            [ SA.cx "24"
            , SA.cy "24"
            , SA.stroke "#6699AA"
            , SA.strokeWidth "4"
            , SA.r "8"
            , SA.fill "none"
            ]
            [ S.animate
                [ SA.attributeName "opacity"
                , SA.values "0;.8;0"
                , SA.dur "2s"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        ]


viewErrorMessage : Model -> E.Element Msg
viewErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            E.text errorMessage

        Nothing ->
            E.none


viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.row [ E.spacing 10, E.paddingXY 0 12 ]
        [ EI.search []
            { label = EI.labelLeft [] (E.text "Search Books: ")
            , onChange = MsgInputTextField
            , placeholder = Nothing
            , text = model.searchText
            }
        , viewSearchButton
        , if model.loading then
            E.html loadingImage

          else
            E.none
        ]


viewSearchButton : E.Element Msg
viewSearchButton =
    EI.button
        [ EBG.color (E.rgb255 0x00 0x33 0x66)
        , EF.color (E.rgb255 0xEE 0xEE 0xEE)
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just MsgSearch
        , label = E.text "Search"
        }


viewResults : Model -> E.Element msg
viewResults model =
    E.wrappedRow [ E.spacing 5, E.centerX ]
        (List.map viewBook model.results)


viewBook : Book -> E.Element msg
viewBook book =
    let
        title =
            Maybe.withDefault "{ No title provided }" book.title

        titleE =
            E.paragraph [ EF.bold, EF.underline, E.paddingXY 0 12 ] [ E.text title ]

        thumbnailE =
            case book.thumbnail of
                Just thumbnail ->
                    viewBookCover thumbnail title

                Nothing ->
                    E.none

        pagesE =
            E.paragraph [ EF.size 12 ] [ E.text (Maybe.withDefault "No information" (Maybe.map String.fromInt book.pages) ++ " pages") ]

        publisherE =
            case book.publisher of
                Just publisher ->
                    E.paragraph [ EF.size 16 ] [ E.text publisher ]

                Nothing ->
                    E.none
    in
    E.newTabLink
        [ E.width (E.px 360)
        , E.height (E.px 300)
        , EBG.color (E.rgb255 0xE3 0xEA 0xED)
        , EB.rounded 20
        , E.padding 10
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            ]
        ]
        { url = book.link
        , label =
            E.row [ E.centerX ]
                [ thumbnailE
                , E.column [ E.padding 20 ]
                    [ titleE
                    , publisherE
                    , pagesE
                    ]
                ]
        }


viewBookCover : String -> String -> E.Element msg
viewBookCover thumbnail title =
    E.image []
        { src = thumbnail
        , description = title
        }


cmdSearch : Model -> Cmd Msg
cmdSearch model =
    Http.get
        { url = "https://www.googleapis.com/books/v1/volumes?&q=" ++ model.searchText
        , expect = Http.expectJson MsgGotResults decodeJson
        }


decodeJson : JD.Decoder (List Book)
decodeJson =
    JD.field "items" decodeItems


decodeItems : JD.Decoder (List Book)
decodeItems =
    JD.list decodeItem


decodeItem : JD.Decoder Book
decodeItem =
    JD.field "volumeInfo" decodeVolumeInfo


decodeVolumeInfo : JD.Decoder Book
decodeVolumeInfo =
    JD.map5 Book
        (JD.maybe (JD.field "title" JD.string))
        (JD.maybe (JD.field "imageLinks" decodeImageLinks))
        (JD.field "canonicalVolumeLink" JD.string)
        (JD.maybe (JD.field "pageCount" JD.int))
        (JD.maybe (JD.field "publisher" JD.string))


decodeImageLinks : JD.Decoder String
decodeImageLinks =
    JD.field "thumbnail" JD.string
