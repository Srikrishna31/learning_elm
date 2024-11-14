module AboutPage exposing (..)

import Element exposing (Element)
import Element.Font
import Element.Input
import UI


type alias Model =
    { showDetail : Bool
    }


initModel : Model
initModel =
    { showDetail = False
    }


type Msg
    = MsgShowDetailClicked


view : Model -> Element.Element Msg
view model =
    Element.column [ Element.padding 20, Element.Font.size 14 ]
        [ Element.el [ Element.Font.size 22 ] (Element.text "About page")
        , Element.paragraph
            [ Element.paddingXY 0 20
            , Element.Font.size 14
            ]
            [ Element.text "This is a web site for learning about navigation."
            ]
        , Element.paragraph []
            [ viewDetail model.showDetail
            ]
        ]


viewDetail : Bool -> Element.Element Msg
viewDetail showDetail =
    let
        link_attrs =
            [ Element.Font.size 12, Element.Font.bold ]
    in
    if showDetail then
        Element.column []
            [ Element.text "The authors of this web site are amazing!"
            , UI.link link_attrs "/about/hide-detail" "Hide"
            ]

    else
        UI.linkWithAction link_attrs MsgShowDetailClicked "Show more"


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        MsgShowDetailClicked ->
            ( { model | showDetail = True }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
