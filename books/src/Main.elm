module Main exposing (main)

import Browser
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html exposing (Html)
import Http exposing (Error(..))
import Json.Decode as JD


type alias Model =
    { searchText : String
    , results : List Book
    , errorMessage : Maybe String
    }


type alias Book =
    { title : String
    , thumbnail : Maybe String
    , link : String
    , pages : Int
    , publisher : Maybe String
    }


type Msg
    = MsgSearch
    | MsgGotResults (Result Http.Error (List Book))
    | MsgInputTextField String


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
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { searchText = ""
    , results = []
    , errorMessage = Nothing
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
            ( model, cmdSearch model )

        MsgGotResults result ->
            case result of
                Ok data ->
                    ( { model | results = data }, Cmd.none )

                Err error ->
                    case error of
                        NetworkError ->
                            ( { model | errorMessage = Just "Network Error" }, Cmd.none )

                        BadUrl string ->
                            ( { model | errorMessage = Just string }, Cmd.none )

                        Timeout ->
                            ( { model | errorMessage = Just "Request Timed out" }, Cmd.none )

                        BadStatus int ->
                            ( { model | errorMessage = Just "Bad status" }, Cmd.none )

                        BadBody string ->
                            ( { model | errorMessage = Just string }, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (E.column [] [ viewSearchBar model, viewErrorMessage model, viewResults model ])


viewErrorMessage : Model -> E.Element Msg
viewErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            E.text errorMessage

        Nothing ->
            E.none


viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.row []
        [ EI.search []
            { label = EI.labelLeft [] (E.text "Search Books: ")
            , onChange = MsgInputTextField
            , placeholder = Nothing
            , text = model.searchText
            }
        , viewSearchButton
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
        ]
        { onPress = Just MsgSearch
        , label = E.text "Search"
        }


viewResults : Model -> E.Element msg
viewResults model =
    E.column []
        (List.map viewBook model.results)


viewBook : Book -> E.Element msg
viewBook book =
    E.newTabLink []
        { url = book.link
        , label =
            E.column []
                [ E.text book.title
                , case book.thumbnail of
                    Just thumbnail ->
                        viewBookCover thumbnail book.title

                    Nothing ->
                        E.none
                , E.text (String.fromInt book.pages)
                , E.text (Maybe.withDefault "Publisher not found" book.publisher)
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
        { url = "https://www.googleapis.com/books/v1/volumes?q=" ++ model.searchText
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
        (JD.field "title" JD.string)
        (JD.maybe (JD.field "imageLinks" decodeImageLinks))
        (JD.field "canonicalVolumeLink" JD.string)
        (JD.field "pageCount" JD.int)
        (JD.maybe (JD.field "publisher" JD.string))


decodeImageLinks : JD.Decoder String
decodeImageLinks =
    JD.field "thumbnail" JD.string
